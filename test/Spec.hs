import ClassyPrelude
import Data.BmLinear.Config (Allowance(..))
import Data.BmLinear.Scorer (dailyScore, LocalTimeEntry(..), CompletedTimeEntry(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (secondsToDiffTime, UTCTime(..))
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import qualified Network.Hoggl.Types as Hoggl

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

projects :: Map Hoggl.ProjectId Double
projects = Map.fromList [(Hoggl.PID 1, -1.0), (Hoggl.PID 2, 1.0)]

lte = LocalTimeEntry { start = LocalTime (ModifiedJulianDay 0) (TimeOfDay 9 0 0)
                     , underlying =
                       CompletedTimeEntry { teStart = UTCTime (ModifiedJulianDay 0) . secondsToDiffTime $ 9 * 60 * 60
                                          , teStop = UTCTime (ModifiedJulianDay 0) . secondsToDiffTime $ (9 * 60 + 20) * 60
                                          , teProjectId = Just $ Hoggl.PID 1
                                          }
                     }

tests :: TestTree
tests = testCase "dailyScore" $
  assertEqual "" [(-5.0)] $
    dailyScore projects [Allowance (Set.fromList [1..5]) (TimeOfDay 9 0 0) (TimeOfDay 10 0 0) $ 15 * 60] [lte]
