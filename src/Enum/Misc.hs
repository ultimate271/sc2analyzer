module Enum.Misc where

type Mineral = Double
type Vespene = Double
type Time = Int

data Resources = Resources { minerals :: Mineral, vespene :: Vespene }
    deriving (Show, Eq)
instance Ord Resources where
    (<=) (Resources m1 v1) (Resources m2 v2) = m1 <= m2 && v1 <= v2
negate :: Resources -> Resources
negate (Resources m v) = Resources (-1 * m) (-1 * v)
data Workers = Workers { mineralWorkers :: Int, vespeneWorkers :: Int }
    deriving (Show, Eq)

