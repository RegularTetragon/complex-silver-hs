{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module ComplexSilver.World where
import Apecs
import Apecs.Physics
import Apecs.Physics.Gloss
import ComplexSilver.Sprites
import ComplexSilver.Components.CameraTarget
import ComplexSilver.Components.Player


makeWorld "World" [
    ''Camera,
    ''Animation,
    ''Physics,
    ''SpriteSheets,
    ''CameraTarget,
    ''PlayerMovement,
    ''Grounded]