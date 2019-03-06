module Test.GameState
    (
    ) where

import           Enum.Misc
import qualified Enum.Entity as Entity
import           Enum.Entity (Entity)
-- import qualified Enum.Building as Building
-- import           Enum.Building (Building)
-- import qualified Enum.Unit as Unit
-- import           Enum.Unit (Unit)
import qualified Enum.Command as Command
import           Enum.Command (Command)
import qualified Core.CommandQueue as CommandQueue
import           Core.CommandQueue (CommandQueue, CommandQueueFunction)
import qualified Core.GameState as GameState
import qualified BuiltIn.GameState as GameState
import           Core.GameState (GameState)

test1 :: GameState -> Bool
test1 g = workersCount <= probeCount where
    probeCount = length $ filter (== Entity.Probe) $ GameState.entities g
    mineralWorkerCount = mineralWorkers $ GameState.workers g
    vespeneWorkerCount = vespeneWorkers $ GameState.workers g
    workersCount = mineralWorkerCount + vespeneWorkerCount

g1 :: GameState
g1 = GameState.init

cq1 :: CommandQueue
cq1 =
    [ (Command.Build Entity.Probe, 0)
    , (Command.Build Entity.Probe, 12)
    , (Command.Build Entity.Pylon, 15)
    , (Command.Build Entity.Probe, 24)
    , (Command.Build Entity.Probe, 36)
    , (Command.Build Entity.CyberneticsCore, 48)
    , (Command.Build Entity.Probe, 48)
    , (Command.Build Entity.Probe, 60)
    ]

