{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import ClassyPrelude hiding (readFile, writeFile)
import Control.Lens.Getter ((^.))
import Control.Lens.Operators ((&))
import Control.Lens.Setter ((.~), (%~))
import Control.Monad (msum)
import Control.Monad.Log (renderWithSeverity, runLoggingT)
import Control.Monad.Trans.Maybe
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.BmLinear.Config
import Data.BmLinear.Scorer
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Default.Class
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (TimeOfDay)
import Network.Beeminder
import Network.Hoggl (listProjects, listWorkspaces, togglBaseUrl)
import qualified Network.Hoggl.Types as Hoggl
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (ClientEnv(..), runClientM, ServantError)
import System.Directory (doesFileExist, getXdgDirectory, XdgDirectory(XdgConfig))
import System.Process (readProcess)
import Web.HttpApiData (toUrlPiece)

import Prelude (read)

getAllConfigFiles :: FilePath -> FilePath -> IO (NonNull [FilePath])
getAllConfigFiles appName filename =
  omap (</> filename) . (`ncons` ["/etc" </> appName]) <$> getXdgDirectory XdgConfig appName

findM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM f = runMaybeT . msum . map (MaybeT . f)

activeGoals :: Token -> IO (Maybe [Goal])
activeGoals tok = runBeeminder tok $ do
  loggedInUser <- user $ def & _GoalsFilter .~ Just Front & _LevelOfDetail .~ EverythingCurrent
  case loggedInUser ^. _Goals of
    Hashes h -> pure h
    _ -> fail "Unexpected API response"

tryReadConfigs :: [FilePath] -> IO Config
tryReadConfigs =
  maybe (fail "Configuration file not found") pure <=< findM tryReadConfig

tryReadConfig :: FilePath -> IO (Maybe Config)
tryReadConfig fp = do
  exists <- doesFileExist fp
  if exists then do
    putStrLn $ "Reading config from file " ++ pack fp
    config <- readFile fp
    either (\err -> fail $ "Parse error: " ++ err) (pure . pure) $ eitherDecode config
    else do
            putStrLn $ "No configuration file found at " ++ pack fp
            pure Nothing

-- TODO: Support more UIs than just kdialog
kdialog :: String -> String -> String -> [String] -> IO String
kdialog dialogTitle questionOrStatement mode extraArgs = do
  input <- readProcess "kdialog" (["--title", dialogTitle, "--" ++ mode, questionOrStatement] ++ extraArgs) ""
  pure . concat $ lines input

data Selectee = Selectee { selId :: String, selLabel :: String }

class Summarise a where
  summarise :: a -> Selectee

menu :: Summarise a => String -> String -> [a] -> IO String
menu dialogTitle question =
  kdialog dialogTitle question "menu" . ((\Selectee { selId, selLabel } -> [selId, selLabel]) . summarise =<<)

data Selected = Selected | Unselected

instance Show Selected where
  show Selected = "on"
  show Unselected = "off"

radioList :: String -> String -> String -> [Selectee] -> IO String
radioList dialogTitle question defaultAnswerId =
  kdialog dialogTitle question "radiolist" . ((\Selectee { selId, selLabel } -> [selId, selLabel, show $ selectionStatus selId]) =<<)
    where
      selectionStatus :: String -> Selected
      selectionStatus s | s == defaultAnswerId = Selected
      selectionStatus _ = Unselected

mapNonNull :: (a -> b) -> NonNull [a] -> NonNull [b]
mapNonNull f = impureNonNull . map f . toNullable

-- | Like radioList, but automatically selects the first element as the default.
radioList' :: Summarise a => String -> String -> NonNull [a] -> IO String
radioList' dialogTitle question answers =
  let sa :: NonNull [Selectee]
      sa = mapNonNull summarise answers in
  radioList dialogTitle question (selId $ head sa) $ toNullable sa

checkList :: Summarise a => String -> String -> Set String -> [a] -> IO [String]
checkList dialogTitle question defaultSelected =
  map (map read . words) . kdialog dialogTitle question "checklist" . ((\Selectee { selId, selLabel } -> [selId, selLabel, show $ selectionStatus selId]) . summarise =<<)
  where
    selectionStatus :: String -> Selected
    selectionStatus s | s `elem` defaultSelected = Selected
    selectionStatus _ = Unselected

inputBox :: String -> String -> IO String
inputBox dialogTitle question = kdialog dialogTitle question "inputbox" []

yesNo :: String -> String -> IO ()
yesNo dialogTitle question = () <$ kdialog dialogTitle question "yesno" []

msgBox :: String -> String -> IO ()
msgBox dialogTitle statement = () <$ kdialog dialogTitle statement "msgbox" []

instance Summarise Goal where
  summarise Goal { gGoal, gTitle } =
    Selectee { selId = unpack gGoal, selLabel = unpack gGoal ++ ": " ++ unpack gTitle }

instance Summarise Hoggl.Workspace where
  summarise Hoggl.Workspace { Hoggl.wsId = wsId, Hoggl.wsName = wsName } =
    Selectee { selId = unpack $ toUrlPiece wsId, selLabel = unpack $ toUrlPiece wsName }

instance Summarise Hoggl.Project where
  summarise Hoggl.Project { Hoggl.prId = prId, Hoggl.prName = prName } =
    Selectee { selId = unpack $ toUrlPiece prId, selLabel = unpack $ toUrlPiece prName }

fromPID :: Hoggl.ProjectId -> Integer
fromPID (Hoggl.PID i) = i

addGoalCmd :: ClientEnv -> FilePath -> Config -> IO ()
addGoalCmd clientEnv userConfigFile config =
  let beeminderToken = token' $ beeminder config
  in do
    goals <- maybe (fail "Getting goals failed") pure =<< activeGoals beeminderToken
    nnGoals <- maybe (fail "You have not created any goals yet") pure $ fromNullable goals
    goalSlug <- radioList' "bm-linear" "Goal to add" nnGoals
    togglToken <- inputBox "bm-linear" "Enter Toggl.com API token"
    let togglAuth = Hoggl.Api togglToken
    togglWorkspaces <- runClientM (listWorkspaces togglAuth) clientEnv
    let workspaces = either (pure $ error "Toggl API call failed") id togglWorkspaces
    wsIDs' <- checkList "bm-linear" "Which Toggl workspace(s) to include?" mempty workspaces
    let wsIDs = read <$> wsIDs'
    let getProjects :: Hoggl.WorkspaceId -> IO (Either ServantError [Hoggl.Project])
        getProjects wsId = runClientM (listProjects togglAuth wsId) clientEnv
    projects <- concat <$> mapM (either (\e -> fail $ "Toggl API call failed due to " ++ show e) pure <=< getProjects . Hoggl.WID) wsIDs
    when (length projects < 2) $ fail "You must create at least 2 projects for a linear combination"
    prIDs <- map read <$> checkList "bm-linear" "Which Toggl project(s) to include?" mempty projects
    when (length prIDs < 2) $ fail "You must select at least 2 projects for a linear combination. If you don't know how to select multiple items from a list on your operating system, please just Google it."
    let askAboutProject :: Hoggl.ProjectId -> IO (Hoggl.ProjectId, Double)
        askAboutProject prId =
          (prId, ) . read <$> inputBox "bm-linear" ("How much weight to give " ++ unpack projectName ++ "?")
          where
            projectName = fromJust $ Hoggl.prName <$> find ((== prId) . Hoggl.prId) projects
    weightsList <- mapM (askAboutProject . Hoggl.PID) prIDs
    let goalConfig = GoalConfig { togglToken = pack togglToken
                                , togglWorkspaceIds = Set.fromList wsIDs
                                , togglProjects = Map.fromList $ first fromPID <$> weightsList
                                , allowances = []
                                }
    writeFile userConfigFile . encodePretty $ config & _beeminder . _goals %~ Map.insert (pack goalSlug) goalConfig

process :: ClientEnv -> TimeOfDay -> Token -> Text -> GoalConfig -> IO ()
process clientEnv dayStarts bmToken goalSlug config = do
  GoalResult { goalScore, asOf } <- runLoggingT (nextScore clientEnv dayStarts bmToken goalSlug config) (print . renderWithSeverity id)
  yesNo "bm-linear" $ "Your score for " ++ unpack goalSlug ++ " is " ++ show goalScore ++ ". Do you want to upload it to Beeminder now?"
  if goalScore == 0.0 then
    msgBox "bm-linear" "Not uploading 0.0 - no point"
  else do
    result <- runBeeminder bmToken . createPoint $ def & _Goal .~ goalSlug & _Timestamp .~ round (utcTimeToPOSIXSeconds asOf) & _Value .~ goalScore
    maybe (fail "Upload failed") (pure $ msgBox "bm-linear" "Upload succeeded") result

main :: IO ()
main = do
  configFiles <- getAllConfigFiles "bm-linear" "bm-linear.json"
  let userConfigFile = head configFiles
  config <- tryReadConfigs $ toNullable configFiles
  args <- getArgs
  manager <- newManager tlsManagerSettings
  let clientEnv = ClientEnv manager togglBaseUrl
  if args == ["add-goal"] || null (goals $ beeminder config) then
    addGoalCmd clientEnv userConfigFile config
  else
    let bm = beeminder config
    in mapM_ (uncurry . process clientEnv (dayBoundary config) $ token' bm) . Map.toList $ goals bm
