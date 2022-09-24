{-# LANGUAGE TypeFamilies #-}
module ComplexSilver.Components.Pickup where
import Apecs
import Apecs.Physics

data PickupCommand = Activate

data Pickup = UnheldPickup | HeldPickup Entity [PickupCommand]

data PlayerPickupController = NotHoldingAnything {equipTo :: Entity} | HoldingPickup {
    holder :: Entity,
    holdingItem :: Entity
}

instance Component Pickup where
    type Storage Pickup  = Map Pickup

instance Component PlayerPickupController where
    type Storage PlayerPickupController = Map PlayerPickupController