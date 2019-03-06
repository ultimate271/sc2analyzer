module Enum.Buildable where

import Enum.Misc

class Buildable a where
    buildCost :: a -> Resources
    buildTime :: a -> Time
