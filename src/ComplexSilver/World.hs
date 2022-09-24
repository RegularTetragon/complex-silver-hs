{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ComplexSilver.World where

import Apecs
import Apecs.Physics
import Apecs.Physics.Gloss
import ComplexSilver.Components.CameraTarget
import ComplexSilver.Components.Pickup
import ComplexSilver.Components.Player
import ComplexSilver.Components.TerminalVelocity
import ComplexSilver.Components.Facing
import ComplexSilver.Sprites

makeWorld
  "World"
  [ ''Physics,
    ''Camera,
    ''Animation,
    ''SpriteSheets,
    ''CameraTarget,
    ''PlayerMovement,
    ''Grounded,
    ''TerminalVelocity,
    ''Pickup,
    ''PlayerPickupController,
    ''Facing
  ]