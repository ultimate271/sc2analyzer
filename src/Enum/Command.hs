module Enum.Command
    ( Command(..)
    ) where

import           Enum.Misc
import qualified Enum.Building as Building
import           Enum.Building (Building)
import qualified Enum.Unit as Unit
import           Enum.Unit (Unit)
-- import qualified Enum.Command as Command
-- import           Enum.Command (Command)

data Command
    = Build Building
    | WarpIn Unit
    | SetProbes Mineral Vespene
    deriving (Show, Eq)
