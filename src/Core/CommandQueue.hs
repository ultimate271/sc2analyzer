module Core.CommandQueue
    ( CommandQueue
    , CommandQueueFunction
    ) where

import           Enum.Misc
import qualified Enum.Building as Building
import           Enum.Building (Building)
import qualified Enum.Unit as Unit
import           Enum.Unit (Unit)
import qualified Enum.Command as Command
import           Enum.Command (Command)

type CommandQueue = [(Command, Time)]
type CommandQueueFunction = Time -> [Command]

-- | Takes a command queue, and converts it to a function from the time the
-- command takes place to the list of commands that take place at that time
-- slot.
getCommands :: CommandQueue -> CommandQueueFunction
getCommands cq = (\t -> [c | (c, i) <- cq, i == t])




