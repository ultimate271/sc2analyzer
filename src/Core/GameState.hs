module Core.GameState
    ( GameState(..)
    , addUnit
    , addBuilding
    , addProduction
    , addResources
    , organizeWorkers
    ) where

import qualified Builtin.Misc as Misc
import           Enum.Misc
import qualified Enum.Building as Building
import           Enum.Building (Building)
import qualified Enum.Unit as Unit
import           Enum.Unit (Unit)
import qualified Enum.Command as Command
import           Enum.Command (Command)
-- import qualified Core.GameState as GameState
-- import           Core.GameState (GameState)

data GameState = GameState
    { units :: [Unit]
    , buildings :: [Building]
    , production :: [(Building, Time)]
    , bank :: Resources
    , workers :: Workers
    }
    deriving (Show)

addUnit :: Unit -> GameState -> GameState
addUnit u g@GameState{units = us} = g { units = u:us }

addBuilding :: Building -> GameState -> GameState
addBuilding b g@GameState{buildings = bs} = g { buildings = b:bs }

addProduction :: Building -> GameState -> GameState
addProduction b g@GameState{production = ps} = g { production = (b, 0):ps }

addResources :: Resources -> GameState -> GameState
addResources
    r@Resources{minerals = m1, vespene = v1}
    g@GameState{bank = Resources {minerals = m2, vespene = v2}}
    = g { bank = Resources { minerals = m1 + m2, vespene = v1 + v2 } }

organizeWorkers :: Workers -> GameState -> GameState
organizeWorkers w g = g {workers = w}

-- | This function will tell us what happens in one discrete jump from one time
-- to the next. The Command list is the commands that are executed in this
-- state. This will both execute the commands that it is able, but it will also
-- do the things neccessary for the regular time expenditure of one second.
iterate :: GameState -> [Command] -> GameState
iterate g cs =
    (\h -> foldl (execCommand) h cs)
    . produceBuildings
    . collectResources
    $ g

mineralsGathered :: GameState -> Mineral
mineralsGathered g@GameState{bank = Resources {minerals = m}}
    = Misc.minGatherRate * m

vespeneGathered :: GameState -> Vespene
vespeneGathered g@GameState{bank = Resources {vespene = v}}
    = Misc.gasGatherRate * v

collectResources :: GameState -> GameState
collectResources g = addResources
    Resources{minerals = mineralsGathered g, vespene = vespeneGathered g} g

produceBuildings :: GameState -> GameState
produceBuildings g@GameState{buildings = bs, production = ps}
    = g { buildings = completed ++ bs, production = complement} where
    completed = fst <$> filter produced ps
    complement = filter (not . produced) ps

produced :: (Building, Time) -> Bool
produced (b,t) = t >= Building.buildTime b

-- | This function will execute the command on the game state and do nothing
-- else. If the command cannot be executed, the gamestate will remain
-- unchanged.
-- TODO: Require that the required tech be a part of the decision to build.
execCommand :: GameState -> Command -> GameState
execCommand g (Command.Build b)
    = if c <= r && length (filter (== Unit.Probe) us) > 0
    then addResources (negate c) $ addProduction b g
    else g where
    c = Building.cost b
    us = GameState.units g
    bs = GameState.buildings g
    r = GameState.bank g

execCommand g (Command.WarpIn u)
    = if c <= r
    then addResources (negate c) $ addProduction b g
    else g where
    c = Unit.cost u
    us = GameState.units g
    bs = GameState.buildings g
    r = GameState.bank g

-- | Takes a pruned CommandQueue and returns the GameState that comes about at
-- Time t
--getState :: CommandQueue -> Time -> GameState
