module Core.GameState
    ( GameState(..)
    , addEntity
    , addProduction
    , addResources
    , organizeWorkers
    , takeStep
    , getGameState
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
-- import qualified Core.GameState as GameState
-- import           Core.GameState (GameState)

data GameState = GameState
    { entities :: [Entity]
    , production :: [(Entity, Time)]
    , bank :: Resources
    , workers :: Workers
    }
    deriving (Show)

addEntity :: Entity -> GameState -> GameState
addEntity e g@GameState{entities = es} = g { entities = e:es }

addProduction :: Entity -> GameState -> GameState
addProduction e g@GameState{production = ps} = g { production = (e, 0):ps }

addResources :: Resources -> GameState -> GameState
addResources
    r@Resources{minerals = m1, vespene = v1}
    g@GameState{bank = Resources {minerals = m2, vespene = v2}}
    = g { bank = Resources { minerals = m1 + m2, vespene = v1 + v2 } }

organizeWorkers :: Workers -> GameState -> GameState
organizeWorkers w g = g {workers = w}

-- | Given the initial state at t=0, and a time, return the state of the game
-- after running for the time, executing the commands in the command queue.
getGameState :: CommandQueue -> GameState -> Time -> GameState
getGameState cq g t = foldl takeStep g $ cqf <$> [0..t] where
    cqf = CommandQueue.getCommandFunction cq


-- | This function will tell us what happens in one discrete jump from one time
-- to the next. The Command list is the commands that are executed in this
-- state. This will both execute the commands that it is able, but it will also
-- do the things neccessary for the regular time expenditure of one second.
takeStep :: GameState -> [Command] -> GameState
takeStep g cs
    = (\h -> foldl execCommand h cs)
    . collectResources
    . produceEntities
    . advanceProduction
    $ g

-- | The number of minerals gathered in one discrete jump
mineralsGathered :: GameState -> Mineral
mineralsGathered g@GameState{workers = Workers {mineralWorkers = m}}
    = Misc.minGatherRate * fromIntegral m

-- | The number of vespene gathered in one discrete jump
vespeneGathered :: GameState -> Vespene
vespeneGathered g@GameState{workers = Workers {vespeneWorkers = v}}
    = Misc.gasGatherRate * fromIntegral v

-- | Updates the gamestate with the updated mineral and vespene quantities
collectResources :: GameState -> GameState
collectResources g = addResources
    Resources{minerals = mineralsGathered g, vespene = vespeneGathered g} g

-- | Updates the gamestate to place the elements in production into the state.
produceEntities :: GameState -> GameState
produceEntities g@GameState{entities = es, production = ps}
    = g { entities = completed ++ es, production = complement} where
    completed = fst <$> filter produced ps
    complement = filter (not . produced) ps

-- | Decides whether or not an entity that is in production has been fully
-- produced.
produced :: (Entity, Time) -> Bool
produced (e,t) = t >= Entity.buildTime e

advanceProduction :: GameState -> GameState
advanceProduction g@GameState{production = ps}
    = g { production = map (\(e, t) -> (e, t + 1)) ps }

-- | Will take all unoccupied probes and place them on minerals
assignProbes :: GameState -> GameState
assignProbes g@GameState
    { entities = es
    , workers = Workers {mineralWorkers = m, vespeneWorkers = v}
    } = organizeWorkers (Workers {mineralWorkers = m', vespeneWorkers = v'}) g where
    probeCount = length $ filter (== Entity.Probe) es
    m' = probeCount - v
    v' = v

-- | This function will execute the command on the game state and do nothing
-- else. If the command cannot be executed, the gamestate will remain
-- unchanged.
execCommand :: GameState -> Command -> GameState
execCommand g (Command.Build b)
    = if c <= r && requirements
    then addResources (Enum.Misc.negate c) $ addProduction b g
    else g where
    c = Entity.buildCost b
    es = entities g
    r = bank g
    requirements = and $ map (\a -> elem a es) $ Entity.buildRequirement b

execCommand g (Command.SetProbes (Workers m v))
    = if m + v <= probeCount
    then organizeWorkers (Workers m v) g
    else g where
    probeCount = length $ filter (== Entity.Probe) $ entities g

-- execCommand g (Command.WarpIn u)
--     = if c <= r
--     then addResources (negate c) $ addProduction b g
--     else g where
--     c = Unit.cost u
--     us = GameState.units g
--     bs = GameState.buildings g
--     r = GameState.bank g


-- | Takes a pruned CommandQueue and returns the GameState that comes about at
-- Time t
--getState :: CommandQueue -> Time -> GameState
