{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.BmLinear.Scorer where

import ClassyPrelude
import Control.Lens.Fold (filtered)
import Control.Lens.Getter (view)
import Control.Lens.Iso (from, Iso', iso)
import Control.Lens.Operators ((&))
import Control.Lens.Setter ((.~), (%~))
import Control.Lens.TH
import Control.Lens.Traversal (mapAccumLOf)
import Control.Monad.Log
import Control.Monad.Trans.State.Lazy (evalState, State, state)
import Data.BmLinear.Config (Allowance(..), allowances, GoalConfig, togglToken', togglProjects', _ignore, _startTime, _endTime)
import Data.Default.Class
import Data.Time (UTCTime)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (addUTCTime, DiffTime, diffUTCTime, getCurrentTime, NominalDiffTime)
import Data.Time.Clock.POSIX
import Data.Time.LocalTime (getCurrentTimeZone, localDay, LocalTime(..), localTimeOfDay, TimeOfDay, timeOfDayToTime, timeToTimeOfDay, TimeZone, utcToLocalTime)
import Network.Beeminder
import Network.Hoggl (getEntries)
import qualified Network.Hoggl.Types as Hoggl
import Servant.Client (ClientEnv(..), runClientM)
import Text.PrettyPrint.Leijen.Text (Doc, text, vsep, (<+>))

data CompletedTimeEntry = CompletedTimeEntry { teStart :: UTCTime
                                             , teStop  :: UTCTime
                                             , teProjectId :: Maybe Hoggl.ProjectId
                                             } deriving (Show)
$(makeLensesFor [ ("teStart", "_teStart")
                , ("teStop", "_teStop")
                ] ''CompletedTimeEntry)

lastTimeDataEntered :: Token -> Text -> IO (Maybe UTCTime)
lastTimeDataEntered tok goalID = runBeeminder tok $ do
  goalDetails <- goal $ def & _Goal .~ goalID
  pure $ posixSecondsToUTCTime . fromIntegral $ gReportedDate goalDetails

data GoalResult = GoalResult { goalScore :: Double
                             , asOf :: UTCTime
                             }

maxEntries :: Int
maxEntries = 999

value :: Show a => a -> Doc
value = text . pack . show

nextScore :: ClientEnv -> TimeOfDay -> Token -> Text -> GoalConfig -> LoggingT (WithSeverity Doc) IO GoalResult
nextScore clientEnv dayStarts tok goalSlug config = do
  ltde <- maybe (fail "Getting goal failed") pure =<< liftIO (lastTimeDataEntered tok goalSlug)
  logDebug $ "ltde =" <+> value ltde
  beforeFetch <- liftIO getCurrentTime
  tz <- liftIO getCurrentTimeZone
  togglEntries <- liftIO $ runClientM (getEntries (togglToken' config) (Hoggl.ISO6801 ltde) (Hoggl.ISO6801 beforeFetch)) clientEnv
  allowances' <- liftIO $ mapM (alAsDayStartingAt dayStarts) $ allowances config
  logDebug $ "allowances' =" <+> value allowances'
  let entries = either (\e -> error $ "Toggl API call failed due to " ++ show e) (toLocalTimeEntry tz . teAsDayStartingAt dayStarts . complete beforeFetch <$>) togglEntries
      entriesByDay = groupBy ((==) `on` start) entries
      entryResults = do
        dayEntries <- entriesByDay
        dailyScore (togglProjects' config) allowances' dayEntries
  when (length entries > maxEntries) . fail $ "Downloading more than " ++ show maxEntries ++ " Toggl time entries is not supported"
  logDebug . vsep $ value <$> entries
  logDebug $ "entryResults =" <+> vsep (value <$> entryResults)
  pure GoalResult { goalScore = sum entryResults, asOf = beforeFetch }

dailyScore :: Traversable t => Map Hoggl.ProjectId Double -> [Allowance] -> t LocalTimeEntry -> t Double
dailyScore projects allowances' dayEntries =
  evalState (mapM (accum projects) dayEntries) allowances'

alAsDayStartingAt :: TimeOfDay -> Allowance -> IO Allowance
alAsDayStartingAt dayStarts al@Allowance { startTime } = do
  when (startTime < dayStarts) . fail $
    "Allowance time periods starting before the day starts are not supported yet. Day starts at " ++ show dayStarts ++ " but an allowance period starts at " ++ show startTime
  let adjust = (`todMinusTod` dayStarts)
  pure $ al & _startTime %~ adjust & _endTime %~ adjust

todMinusTod :: TimeOfDay -> TimeOfDay -> TimeOfDay
todMinusTod t2 t1 =
  view timeTodIso . wrapAround $ view (from timeTodIso) t2 - view (from timeTodIso) t1

wrapAround :: DiffTime -> DiffTime
wrapAround dt | dt < 0 = dt + 60 * 60 * 24
              | otherwise = dt

timeTodIso :: Iso' DiffTime TimeOfDay
timeTodIso = iso timeToTimeOfDay timeOfDayToTime

complete :: UTCTime -> Hoggl.TimeEntry -> CompletedTimeEntry
complete defaultStop Hoggl.TimeEntry { Hoggl.teStart = Hoggl.ISO6801 teStart
                                     , Hoggl.teStop = teStop
                                     , Hoggl.teProjectId = teProjectId } =
  CompletedTimeEntry { teStart
                     , teStop = maybe defaultStop unISO6801 teStop
                     , teProjectId
                     }

unISO6801 :: Hoggl.ISO6801 -> UTCTime
unISO6801 (Hoggl.ISO6801 t) = t

teAsDayStartingAt :: TimeOfDay -> CompletedTimeEntry -> CompletedTimeEntry
teAsDayStartingAt tod te = te & _teStart %~ asDayStartingAt tod & _teStop %~ asDayStartingAt tod

asDayStartingAt :: TimeOfDay -> UTCTime -> UTCTime
asDayStartingAt tod = addUTCTime (negate . diffTimeToNominalDiffTime $ view (from timeTodIso) tod)

diffTimeToNominalDiffTime :: DiffTime -> NominalDiffTime
diffTimeToNominalDiffTime = realToFrac
  
-- | CompletedTimeEntry with cached conversions to LocalTimes
data LocalTimeEntry = LocalTimeEntry { start :: LocalTime
--                                     , stop :: LocalTime
                                     , underlying :: CompletedTimeEntry
                                     } deriving (Show)

toLocalTimeEntry :: TimeZone -> CompletedTimeEntry -> LocalTimeEntry
toLocalTimeEntry tz te@CompletedTimeEntry { teStart
--                                          , teStop
                                          } =
  LocalTimeEntry { start = utcToLocalTime tz teStart
--                 , stop = utcToLocalTime tz teStop
                 , underlying = te
                 }

accum :: Map Hoggl.ProjectId Double -> LocalTimeEntry -> State [Allowance] Double
accum projects LocalTimeEntry { start
                              , underlying = CompletedTimeEntry { teProjectId = projectId
                                                                , teStart
                                                                , teStop
                                                                }
                              } =
  maybe (pure 0) score $ (`lookup` projects) =<< projectId
  where
    score :: Double -> State [Allowance] Double
    score weight = (weight *) . (/ 60.0) . realToFrac <$> state (
      mapAccumLOf (traverse . filtered ((weight < 0 &&) . (start `occursWithin`)) . _ignore) apportion $ diffUTCTime teStop teStart)

occursWithin :: LocalTime -> Allowance -> Bool
occursWithin localTime Allowance { daysOfWeek, startTime, endTime } =
  let localTOD = localTimeOfDay localTime
  in toDayOfWeek (localDay localTime) `elem` daysOfWeek && localTOD >= startTime && localTOD <= endTime

apportion :: (Ord a, Num a) => a -> a -> (a, a)
apportion dur i | i >= dur = (0, i - dur)
                | otherwise = (dur - i, 0)

toDayOfWeek :: Day -> Int
toDayOfWeek day = case toWeekDate day of
  (_, _, w) -> w
