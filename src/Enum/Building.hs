module Enum.Building
    ( Building (..)
    , cost
    , buildTime
    ) where

import Enum.Misc

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

cost :: Building -> Resources
cost Nexus            = Resources { minerals = 400, vespene = 000 }
cost Pylon            = Resources { minerals = 100, vespene = 000 }
cost Assimilator      = Resources { minerals = 075, vespene = 000 }
cost Gateway          = Resources { minerals = 150, vespene = 000 }
cost Forge            = Resources { minerals = 150, vespene = 000 }
cost PhotonCannon     = Resources { minerals = 150, vespene = 000 }
cost ShieldBattery    = Resources { minerals = 100, vespene = 000 }
cost CyberneticsCore  = Resources { minerals = 150, vespene = 000 }
cost TwilightCouncil  = Resources { minerals = 150, vespene = 100 }
cost RoboticsFacility = Resources { minerals = 200, vespene = 100 }
cost Stargate         = Resources { minerals = 150, vespene = 150 }
cost TemplarArchives  = Resources { minerals = 150, vespene = 200 }
cost DarkShrine       = Resources { minerals = 150, vespene = 150 }
cost RoboticsBay      = Resources { minerals = 150, vespene = 150 }
cost FleetBeacon      = Resources { minerals = 300, vespene = 200 }

buildTime :: Building -> Time
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


