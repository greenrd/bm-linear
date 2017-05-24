{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.BmLinear.Config where

import ClassyPrelude
import Control.Lens.TH
import Data.Aeson.TH
import Data.Map.Strict (mapKeys)
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime)
import Data.Time.LocalTime (TimeOfDay)
import Network.Beeminder (Token)
import qualified Network.Hoggl.Types as Hoggl

-- TODO: Introduce real Day ADT and iso to int
data Allowance = Allowance { daysOfWeek :: Set Int
                           , startTime :: TimeOfDay
                           , endTime :: TimeOfDay
                           , ignore :: NominalDiffTime
                           } deriving (Show)

$(deriveJSON defaultOptions ''Allowance)
$(makeLensesFor [ ("daysOfWeek", "_daysOfWeek")
                , ("startTime", "_startTime")
                , ("endTime", "_endTime")
                , ("ignore", "_ignore")
                ] ''Allowance)

data GoalConfig = GoalConfig { togglToken :: Text
                             , togglWorkspaceIds :: Set Integer
                             , togglProjects :: Map Integer Double
                             , allowances :: [Allowance]
                             } deriving (Show)

deriving instance Ord Hoggl.ProjectId
deriving instance Ord Hoggl.WorkspaceId

togglToken' :: GoalConfig -> Hoggl.Token
togglToken' = Hoggl.Api . unpack . togglToken
togglWorkspaceIds' :: GoalConfig -> Set Hoggl.WorkspaceId
togglWorkspaceIds' = Set.map Hoggl.WID . togglWorkspaceIds
togglProjects' :: GoalConfig -> Map Hoggl.ProjectId Double
togglProjects' = mapKeys Hoggl.PID . togglProjects

$(deriveJSON defaultOptions ''GoalConfig)
$(makeLensesFor [ ("togglToken", "_togglToken")
                , ("togglWorkspaceIds", "_togglWorkspaceIds")
                , ("togglProjects", "_togglProjects")
                ] ''GoalConfig)

data Beeminder = Beeminder { token :: Text
                           , goals :: Map Text GoalConfig 
                           } deriving (Show)

token' :: Beeminder -> Token
token' = encodeUtf8 . token

$(deriveJSON defaultOptions ''Beeminder)
$(makeLensesFor [ ("token", "_token")
                , ("goals", "_goals")
                ] ''Beeminder)

data Config = Config { beeminder :: Beeminder
                     , dayBoundary :: TimeOfDay } deriving (Show)

$(deriveJSON defaultOptions ''Config)
$(makeLensesFor [ ("beeminder", "_beeminder")
                , ("dayBoundary", "_dayBoundary")
                ] ''Config)
