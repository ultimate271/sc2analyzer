module Enum.Entity where

import Enum.Misc

data EntityKind
    = Unit
    | Building
    | Upgrade
data Entity
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
    | Nexus
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

buildCost :: Entity -> Resources
buildCost Probe            = Resources { minerals = 050, vespene = 000 }
buildCost Zealot           = Resources { minerals = 100, vespene = 000 }
buildCost Sentry           = Resources { minerals = 055, vespene = 100 }
buildCost Stalker          = Resources { minerals = 125, vespene = 050 }
buildCost Adept            = Resources { minerals = 100, vespene = 025 }
buildCost HighTemplar      = Resources { minerals = 050, vespene = 150 }
buildCost DarkTemplar      = Resources { minerals = 125, vespene = 125 }
buildCost Archon           = Resources { minerals = 000, vespene = 000 }
buildCost Observer         = Resources { minerals = 025, vespene = 075 }
buildCost WarpPrism        = Resources { minerals = 200, vespene = 000 }
buildCost Immortal         = Resources { minerals = 275, vespene = 100 }
buildCost Colossus         = Resources { minerals = 300, vespene = 200 }
buildCost Disruptor        = Resources { minerals = 150, vespene = 150 }
buildCost Phoenix          = Resources { minerals = 150, vespene = 100 }
buildCost VoidRay          = Resources { minerals = 250, vespene = 150 }
buildCost Oracle           = Resources { minerals = 150, vespene = 150 }
buildCost Tempest          = Resources { minerals = 250, vespene = 175 }
buildCost Carrier          = Resources { minerals = 350, vespene = 250 }
buildCost Mothership       = Resources { minerals = 300, vespene = 300 }
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

buildTime :: Entity -> Time
buildTime Probe            = 12
buildTime Zealot           = 20
buildTime Sentry           = 23
buildTime Stalker          = 23
buildTime Adept            = 20
buildTime HighTemplar      = 32
buildTime DarkTemplar      = 32
buildTime Archon           = 9
buildTime Observer         = 21
buildTime WarpPrism        = 36
buildTime Immortal         = 39
buildTime Colossus         = 54
buildTime Disruptor        = 36
buildTime Phoenix          = 25
buildTime VoidRay          = 43
buildTime Oracle           = 36
buildTime Tempest          = 43
buildTime Carrier          = 64
buildTime Mothership       = 71
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

getKind :: Entity -> EntityKind
getKind Probe            = Unit
getKind Zealot           = Unit
getKind Sentry           = Unit
getKind Stalker          = Unit
getKind Adept            = Unit
getKind HighTemplar      = Unit
getKind DarkTemplar      = Unit
getKind Archon           = Unit
getKind Observer         = Unit
getKind WarpPrism        = Unit
getKind Immortal         = Unit
getKind Colossus         = Unit
getKind Disruptor        = Unit
getKind Phoenix          = Unit
getKind VoidRay          = Unit
getKind Oracle           = Unit
getKind Tempest          = Unit
getKind Carrier          = Unit
getKind Mothership       = Unit
getKind Nexus            = Building
getKind Pylon            = Building
getKind Assimilator      = Building
getKind Gateway          = Building
getKind Forge            = Building
getKind PhotonCannon     = Building
getKind ShieldBattery    = Building
getKind CyberneticsCore  = Building
getKind TwilightCouncil  = Building
getKind RoboticsFacility = Building
getKind Stargate         = Building
getKind TemplarArchives  = Building
getKind DarkShrine       = Building
getKind RoboticsBay      = Building
getKind FleetBeacon      = Building

buildRequirement :: Entity -> [Entity]
buildRequirement Probe            = []
buildRequirement Zealot           = []
buildRequirement Sentry           = [CyberneticsCore]
buildRequirement Stalker          = [CyberneticsCore]
buildRequirement Adept            = [CyberneticsCore]
buildRequirement HighTemplar      = [TemplarArchives]
buildRequirement DarkTemplar      = [DarkShrine]
buildRequirement Archon           = []
buildRequirement Observer         = []
buildRequirement WarpPrism        = []
buildRequirement Immortal         = []
buildRequirement Colossus         = [RoboticsBay]
buildRequirement Disruptor        = [RoboticsBay]
buildRequirement Phoenix          = []
buildRequirement VoidRay          = []
buildRequirement Oracle           = []
buildRequirement Tempest          = [FleetBeacon]
buildRequirement Carrier          = [FleetBeacon]
buildRequirement Mothership       = [FleetBeacon]
buildRequirement Nexus            = [Probe]
buildRequirement Pylon            = [Probe]
buildRequirement Assimilator      = [Probe]
buildRequirement Gateway          = [Probe, Pylon]
buildRequirement Forge            = [Probe, Pylon]
buildRequirement PhotonCannon     = [Probe, Pylon, Forge]
buildRequirement ShieldBattery    = [Probe, Pylon, CyberneticsCore]
buildRequirement CyberneticsCore  = [Probe, Pylon, Gateway]
buildRequirement TwilightCouncil  = [Probe, Pylon, CyberneticsCore]
buildRequirement RoboticsFacility = [Probe, Pylon, CyberneticsCore]
buildRequirement Stargate         = [Probe, Pylon, CyberneticsCore]
buildRequirement TemplarArchives  = [Probe, Pylon, TwilightCouncil]
buildRequirement DarkShrine       = [Probe, Pylon, TwilightCouncil]
buildRequirement RoboticsBay      = [Probe, Pylon, RoboticsFacility]
buildRequirement FleetBeacon      = [Probe, Pylon, Stargate]

builtIn :: Entity -> Maybe Entity
builtIn Probe            = Just Nexus
builtIn Zealot           = Just Gateway
builtIn Sentry           = Just Gateway
builtIn Stalker          = Just Gateway
builtIn Adept            = Just Gateway
builtIn HighTemplar      = Just Gateway
builtIn DarkTemplar      = Just Gateway
builtIn Archon           = Nothing
builtIn Observer         = Just RoboticsFacility
builtIn WarpPrism        = Just RoboticsFacility
builtIn Immortal         = Just RoboticsFacility
builtIn Colossus         = Just RoboticsFacility
builtIn Disruptor        = Just RoboticsFacility
builtIn Phoenix          = Just Stargate
builtIn VoidRay          = Just Stargate
builtIn Oracle           = Just Stargate
builtIn Tempest          = Just Stargate
builtIn Carrier          = Just Stargate
builtIn Mothership       = Just Nexus
builtIn Nexus            = Nothing
builtIn Pylon            = Nothing
builtIn Assimilator      = Nothing
builtIn Gateway          = Nothing
builtIn Forge            = Nothing
builtIn PhotonCannon     = Nothing
builtIn ShieldBattery    = Nothing
builtIn CyberneticsCore  = Nothing
builtIn TwilightCouncil  = Nothing
builtIn RoboticsFacility = Nothing
builtIn Stargate         = Nothing
builtIn TemplarArchives  = Nothing
builtIn DarkShrine       = Nothing
builtIn RoboticsBay      = Nothing
builtIn FleetBeacon      = Nothing

supplyCost :: Entity -> Supply
supplyCost Probe            = 1
supplyCost Zealot           = 2
supplyCost Sentry           = 2
supplyCost Stalker          = 2
supplyCost Adept            = 2
supplyCost HighTemplar      = 2
supplyCost DarkTemplar      = 2
supplyCost Archon           = 4
supplyCost Observer         = 1
supplyCost WarpPrism        = 2
supplyCost Immortal         = 4
supplyCost Colossus         = 6
supplyCost Disruptor        = 3
supplyCost Phoenix          = 2
supplyCost VoidRay          = 4
supplyCost Oracle           = 3
supplyCost Tempest          = 5
supplyCost Carrier          = 6
supplyCost Mothership       = 8
supplyCost Nexus            = -15
supplyCost Pylon            = -8
supplyCost Assimilator      = 0
supplyCost Gateway          = 0
supplyCost Forge            = 0
supplyCost PhotonCannon     = 0
supplyCost ShieldBattery    = 0
supplyCost CyberneticsCore  = 0
supplyCost TwilightCouncil  = 0
supplyCost RoboticsFacility = 0
supplyCost Stargate         = 0
supplyCost TemplarArchives  = 0
supplyCost DarkShrine       = 0
supplyCost RoboticsBay      = 0
supplyCost FleetBeacon      = 0

supplyCap :: [Entity] -> Supply
supplyCap es = sum $ [-x | e <- es, let x = supplyCost e, x < 0]

supplyAcquired :: [Entity] -> Supply
supplyAcquired es = sum $ [x | e <- es, let x = supplyCost e, x > 0]

totalSupply :: [Entity] -> Supply
totalSupply es = sum $ map supplyCost es




