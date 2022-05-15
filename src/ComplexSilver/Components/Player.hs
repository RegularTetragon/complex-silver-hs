{-# LANGUAGE TypeFamilies #-}
module ComplexSilver.Components.Player where
import Apecs
data PlayerMovement = PlayerMovement {
    movingLeft :: Bool,
    movingRight :: Bool,
    jumping :: Bool
} deriving (Show)

instance Component PlayerMovement where
    type Storage PlayerMovement = Unique PlayerMovement