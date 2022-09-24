{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
  ( someFunc,
  )
where

import Apecs
import Apecs.Physics
import Apecs.Physics.Gloss
import ComplexSilver.Components.Player
import ComplexSilver.Constants (resourcePath)
import ComplexSilver.Level
import ComplexSilver.Sprites (Animation (..), SpriteSheets (SpriteSheets), readSpritemap)
import ComplexSilver.Systems.Animation
import ComplexSilver.Systems.CameraTarget (stepCamera)
import ComplexSilver.Systems.Grounded
import ComplexSilver.Systems.Pickup (pickupOnPlayer, stepHeldPickups)
import ComplexSilver.Systems.Player
import ComplexSilver.Systems.Spritesheet (loadSprite, sprite_sheet_0)
import ComplexSilver.Systems.TerminalVelocity (stepTerminal)
import ComplexSilver.World
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)
import GHC.Float
import Linear (V2 (..))
import System.Exit

collisionFilter = CollisionFilter 1 maskAll maskAll

material = (Friction 0.4, Elasticity 0.2, Density 1)

triangle = Line [(0, 16), (-16, -16), (16, -16), (0, 16)]

square = Line [(-16, -16), (16, -16), (16, 16), (-16, 16), (-16, -16)]

debugPhysics = True

draw :: SystemT World IO Picture
draw = do
  picA <-
    foldDraw $
      \( Animation
           { animReel = reel,
             animState = frame,
             animFlipX = flipX,
             animFlipY = flipY
           },
         Position pos,
         Angle th
         ) ->
          let conditionalFlip = scale (if flipX then -1 else 1) (if flipY then -1 else 1)
              translate' (V2 x y) = translate (double2Float x) (double2Float y)
              rotate' th = rotate . double2Float
           in translate' pos $ rotate (double2Float th) $ conditionalFlip $ reel !! frame

  picB <- foldDrawM drawBody
  return $ (if debugPhysics then picB else mempty) <> picA

step :: Double -> SystemT World IO ()
step dT = do
  cmap $ \(_ :: Shape) -> material
  sequence_ $
    ($dT)
      <$> [ stepPhysics,
            stepHeldPickups,
            stepAnimation,
            stepPlayer,
            stepCamera,
            stepGrounded,
            stepTerminal
          ]

handleEvent :: Event -> SystemT World IO ()
handleEvent (EventKey key state modifiers (x, y)) = do
  when (key == Char 'a' || key == Char 'A') $
    cmap (\movement -> movement {movingLeft = state == Down})
  when (key == Char 'd' || key == Char 'D') $
    cmap (\movement -> movement {movingRight = state == Down})
  when (key == SpecialKey KeySpace) $
    cmap (\movement -> movement {jumping = state == Down})
  when (key == Char 'e' && state == Down) $
    pickupOnPlayer
handleEvent e = return ()

-- handleEvent e = liftIO $ print e

initialize :: SystemT World IO ()
initialize = do
  set global $ Gravity (V2 0 (-800))
  set global $ SpriteSheets []
  set global $ Camera 0 4

display = InWindow "Complex-Silver" (1920, 1080) (10, 10)

someFunc :: IO ()
someFunc = do
  w <- initWorld
  runWith w $ do
    initialize
    -- consBrick 0
    loadLevel $ resourcePath <> "/tiled/maps/untitled.tmx"
    play display black 60 draw handleEvent $ step . float2Double