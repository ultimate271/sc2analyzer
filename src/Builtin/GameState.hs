module BuiltIn.GameState where

import qualified Builtin.Misc as Misc
import           Enum.Misc
-- import qualified Enum.Building as Building
-- import           Enum.Building (Building)
-- import qualified Enum.Unit as Unit
-- import           Enum.Unit (Unit)
import qualified Enum.Entity as Entity
import           Enum.Entity (Entity)
import qualified Enum.Command as Command
import           Enum.Command (Command)
import qualified Core.GameState as GameState
import           Core.GameState (GameState(..))
--import qualified BuiltIn.GameState as GameState

zero :: GameState
zero = GameState
    { entities = []
    , production = []
    , bank = Resources {minerals = 0, vespene = 0}
    , workers = Workers {mineralWorkers = 0, vespeneWorkers = 0}
    }

-- | 12 probes, all on minerals, 50 minerals
init :: GameState
init =
    GameState.organizeWorkers Workers{mineralWorkers = 12, vespeneWorkers = 0} $
    GameState.addResources Resources{minerals = 50, vespene = 0} $
    GameState.addEntity Entity.Nexus $
    foldr (GameState.addEntity) zero $ replicate 12 Entity.Probe
