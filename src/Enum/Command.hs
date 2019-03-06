module Enum.Command
    ( Command(..)
    ) where

import           Enum.Misc
import           Enum.Buildable
-- import qualified Enum.Building as Building
-- import           Enum.Building (Building)
import qualified Enum.Entity as Entity
import           Enum.Entity (Entity)
-- import qualified Enum.Command as Command
-- import           Enum.Command (Command)

data Command
    = Build Entity
    | SetProbes Workers
    deriving (Show, Eq)
