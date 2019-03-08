module Processor
    (
    ) where

import qualified Builtin.Misc as Misc
import           Enum.Misc
import           Enum.Buildable
-- import qualified Enum.Building as Building
-- import           Enum.Building (Building)
-- import qualified Enum.Unit as Unit
-- import           Enum.Unit (Unit)
import qualified Enum.Entity as Entity
import           Enum.Entity (Entity)
import qualified Enum.Command as Command
import           Enum.Command (Command)
import qualified Core.CommandQueue as CommandQueue
import           Core.CommandQueue (CommandQueue, CommandQueueFunction)
import qualified Core.GameState as GameState
import           Core.GameState (GameState)

data Processor = Processor CommandQueue GameState
