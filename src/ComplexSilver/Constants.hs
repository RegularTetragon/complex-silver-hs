{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module ComplexSilver.Constants where
import Apecs.Physics (Bitmask (Bitmask), CollisionFilter (..))
import Data.Bits (bit, (.|.))
resourcePath = "resources/"



colLayerWorld = Bitmask $ bit 0
colLayerPlayer = Bitmask $ bit 1
colLayerEnemy = Bitmask $ bit 2
colLayerPlayerProjectile = Bitmask $ bit 3
colLayerPickup = Bitmask $ bit 4
colLayerPickupScanner = Bitmask $ bit 5

solidFilter = CollisionFilter {
  filterGroup = 69,
  filterCategories = colLayerWorld,
  filterMask = colLayerPlayer .|. colLayerPickup
}

playerFilter = CollisionFilter {
  filterGroup = 420,
  filterCategories = colLayerPlayer,
  filterMask = colLayerWorld
}

pickupFilter = CollisionFilter {
    filterGroup = 1,
    filterCategories = colLayerPickup,
    filterMask = colLayerPickupScanner .|. colLayerWorld
}