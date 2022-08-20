{-# LANGUAGE TypeFamilies #-}
module ComplexSilver.Components.Player where
import Apecs
data PlayerMovement = PlayerMovement {
    movingLeft :: Bool,
    movingRight :: Bool,
    jumping :: Bool,
    jumpTimer :: Double
} deriving (Show)

newtype Grounded = Grounded Bool

instance Component PlayerMovement where
    type Storage PlayerMovement = Unique PlayerMovement

instance Component Grounded where
    type Storage Grounded = Map Grounded