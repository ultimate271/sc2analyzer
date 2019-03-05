module Test.GameState
    (
    ) where

import           Enum.Misc
import qualified Enum.Building as Building
import           Enum.Building (Building)
import qualified Enum.Unit as Unit
import           Enum.Unit (Unit)
import qualified Enum.Command as Command
import           Enum.Command (Command)
import qualified Core.GameState as GameState
import qualified BuiltIn.GameState as GameState
import           Core.GameState (GameState)

test1 :: GameState -> Bool
test1 g = workersCount <= probeCount where
    probeCount = length $ filter (== Unit.Probe) $ GameState.units g
    mineralWorkerCount = mineralWorkers $ GameState.workers g
    vespeneWorkerCount = vespeneWorkers $ GameState.workers g
    workersCount = mineralWorkerCount + vespeneWorkerCount


