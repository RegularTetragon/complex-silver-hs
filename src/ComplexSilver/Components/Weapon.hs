{-# LANGUAGE TypeFamilies #-}
module ComplexSilver.Components.Weapons where
import Apecs.Physics (Bitmask)
import Apecs

data Projectile = Bullet | Rocket | Explosion

newtype WeaponFireIntent = FireIntent Bool
type Interval = Float
type Timer = Float

data UsageTimer  = UsageTimer Interval Timer
data WeaponAmmo  = Infinite | Ammo Int
data UsageAction = CreateProjectile Projectile | MeleeAttack | NoAction

data Weapon = Weapon UsageTimer WeaponAmmo UsageAction

instance Component Weapon where
  type Storage Weapon = Map Weapon