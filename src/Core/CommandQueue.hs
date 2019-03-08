module Core.CommandQueue
    ( CommandQueue
    , CommandQueueFunction
    , getCommandFunction
    ) where

import           Enum.Misc
-- import qualified Enum.Building as Building
-- import           Enum.Building (Building)
-- import qualified Enum.Unit as Unit
-- import           Enum.Unit (Unit)
import qualified Enum.Entity as Entity
import           Enum.Entity (Entity)
import qualified Enum.Command as Command
import           Enum.Command (Command)

type CommandQueue = [(Command, Time)]
type CommandQueueFunction = Time -> [Command]

-- | Takes a command queue, and converts it to a function from the time the
-- command takes place to the list of commands that take place at that time
-- slot.
getCommandFunction :: CommandQueue -> CommandQueueFunction
getCommandFunction cq = (\t -> [c | (c, i) <- cq, i == t])

-- | TODO: THIS IS A BAD WAY TO DO THINGS
--prune :: (Eq a) => (CommandQueue -> a -> Time -> a) -> a -> ComamndQueue -> CommandQueue
--prune _ _ [] = []
--prune f a0 ((c, t):cts)@cq = 
--    if f cq a0 t == f cts a0 t
--    then prune f (f cq a0 t) cts
--    else where
--    remaining = prune f (f cq a0 t)
--prune f a0 cq = filter 

--foldl (\p t -> (f (fst p) (cqf t), cqf t) (g, []) [0..maxTime]
--
--g (cqf <$> [0..maxTime])
--    maxTime = max $ snd <$> cq
--    cgf = getCommandFunction cq
--

