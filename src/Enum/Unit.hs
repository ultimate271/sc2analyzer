module Enum.Unit where

import Enum.Misc

data Unit
    = Probe
    | Zealot
    | Sentry
    | Stalker
    | Adept
    | HighTemplar
    | DarkTemplar
    | Archon
    | Observer
    | WarpPrism
    | Immortal
    | Colossus
    | Disruptor
    | Phoenix
    | VoidRay
    | Oracle
    | Tempest
    | Carrier
    | Mothership
    deriving (Show, Eq)

cost :: Unit -> Resources
cost Probe       = Resources { minerals = 050, vespene = 000 }
cost Zealot      = Resources { minerals = 100, vespene = 000 }
cost Sentry      = Resources { minerals = 055, vespene = 100 }
cost Stalker     = Resources { minerals = 125, vespene = 050 }
cost Adept       = Resources { minerals = 100, vespene = 025 }
cost HighTemplar = Resources { minerals = 050, vespene = 150 }
cost DarkTemplar = Resources { minerals = 125, vespene = 125 }
cost Archon      = Resources { minerals = 000, vespene = 000 }
cost Observer    = Resources { minerals = 025, vespene = 075 }
cost WarpPrism   = Resources { minerals = 200, vespene = 000 }
cost Immortal    = Resources { minerals = 275, vespene = 100 }
cost Colossus    = Resources { minerals = 300, vespene = 200 }
cost Disruptor   = Resources { minerals = 150, vespene = 150 }
cost Phoenix     = Resources { minerals = 150, vespene = 100 }
cost VoidRay     = Resources { minerals = 250, vespene = 150 }
cost Oracle      = Resources { minerals = 150, vespene = 150 }
cost Tempest     = Resources { minerals = 250, vespene = 175 }
cost Carrier     = Resources { minerals = 350, vespene = 250 }
cost Mothership  = Resources { minerals = 300, vespene = 300 }

buildTime :: Unit -> Time
buildTime Probe       = 12
buildTime Zealot      = 20
buildTime Sentry      = 23
buildTime Stalker     = 23
buildTime Adept       = 20
buildTime HighTemplar = 32
buildTime DarkTemplar = 32
buildTime Archon      = 9
buildTime Observer    = 21
buildTime WarpPrism   = 36
buildTime Immortal    = 39
buildTime Colossus    = 54
buildTime Disruptor   = 36
buildTime Phoenix     = 25
buildTime VoidRay     = 43
buildTime Oracle      = 36
buildTime Tempest     = 43
buildTime Carrier     = 64
buildTime Mothership  = 71
