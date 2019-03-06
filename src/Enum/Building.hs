module Enum.Building
    ( Building (..),
    buildCost,
    buildTime
    ) where

import Enum.Misc
import Enum.Buildable

data Building
    = Nexus
    | Pylon
    | Assimilator
    | Gateway
    | Forge
    | PhotonCannon
    | ShieldBattery
    | CyberneticsCore
    | TwilightCouncil
    | RoboticsFacility
    | Stargate
    | TemplarArchives
    | DarkShrine
    | RoboticsBay
    | FleetBeacon
    deriving (Show, Eq)
instance Buildable Building where
    buildCost Nexus            = Resources { minerals = 400, vespene = 000 }
    buildCost Pylon            = Resources { minerals = 100, vespene = 000 }
    buildCost Assimilator      = Resources { minerals = 075, vespene = 000 }
    buildCost Gateway          = Resources { minerals = 150, vespene = 000 }
    buildCost Forge            = Resources { minerals = 150, vespene = 000 }
    buildCost PhotonCannon     = Resources { minerals = 150, vespene = 000 }
    buildCost ShieldBattery    = Resources { minerals = 100, vespene = 000 }
    buildCost CyberneticsCore  = Resources { minerals = 150, vespene = 000 }
    buildCost TwilightCouncil  = Resources { minerals = 150, vespene = 100 }
    buildCost RoboticsFacility = Resources { minerals = 200, vespene = 100 }
    buildCost Stargate         = Resources { minerals = 150, vespene = 150 }
    buildCost TemplarArchives  = Resources { minerals = 150, vespene = 200 }
    buildCost DarkShrine       = Resources { minerals = 150, vespene = 150 }
    buildCost RoboticsBay      = Resources { minerals = 150, vespene = 150 }
    buildCost FleetBeacon      = Resources { minerals = 300, vespene = 200 }

    buildTime Nexus            = 71
    buildTime Pylon            = 18
    buildTime Assimilator      = 21
    buildTime Gateway          = 46
    buildTime Forge            = 32
    buildTime PhotonCannon     = 29
    buildTime ShieldBattery    = 29
    buildTime CyberneticsCore  = 36
    buildTime TwilightCouncil  = 36
    buildTime RoboticsFacility = 46
    buildTime Stargate         = 43
    buildTime TemplarArchives  = 36
    buildTime DarkShrine       = 71
    buildTime RoboticsBay      = 46
    buildTime FleetBeacon      = 43


