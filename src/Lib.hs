{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Lib
    ( someFunc
    ) where
import Apecs
import Apecs.Physics
import Apecs.Physics.Gloss
import Linear (V2(..))
import ComplexSilver.Sprites (readSpritemap, Animation (..), SpriteSheets (SpriteSheets))
import ComplexSilver.World
import ComplexSilver.Level
import qualified Control.Monad.Trans.State as St

import System.Exit
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)
import GHC.Float
import ComplexSilver.Systems.Animation
import ComplexSilver.Systems.Spritesheet (loadSprite, sprite_sheet_0)
import ComplexSilver.Constants (resourcePath)
import qualified Control.Monad.Trans.State as St
import ComplexSilver.Systems.CameraTarget (stepCamera)
import ComplexSilver.Components.Player
import ComplexSilver.Systems.Player

collisionFilter = CollisionFilter 1 maskAll maskAll
material = (Friction 0.4, Elasticity 0.2, Density 1)

triangle = Line [(0,16), (-16, -16), (16, -16), (0,16)]
square = Line[(-16,-16), (16,-16), (16,16), (-16,16), (-16,-16)]


draw :: SystemT World IO Picture
draw = foldDraw $ \(animation :: Animation, pos :: Position) ->
        color white . translate' pos $ animReel animation !! animState animation
    where
        translate' (Position (V2 x y)) = translate (double2Float x) (double2Float y)


step :: Double -> SystemT World IO ()
step dT = do
    cmap $ \(_::Shape) -> material
    sequence_ $ ($dT) <$> [
        stepAnimation,
        stepPlayer,
        stepPhysics,
        stepCamera
        ]


handleEvent :: Event -> SystemT World IO ()
handleEvent (EventKey key state modifiers (x,y)) = do
    when (key == Char 'a' || key == Char 'A') $
        cmap (\movement->movement { movingLeft = state == Down })
    when (key == Char 'd' || key == Char 'D') $ 
        cmap (\movement->movement { movingRight = state == Down })
    when (key == SpecialKey KeySpace) $ 
        cmap (\movement->movement { jumping = state == Down })

handleEvent e = liftIO $ print e

initialize :: SystemT World IO ()
initialize = do
    set global (Gravity (V2 0 (-200)))
    set global $ SpriteSheets []
    set global $ Camera 0 4
    return ()

display = InWindow "Complex-Silver" (1280, 720) (10, 10)

someFunc :: IO ()
someFunc = do
    w <- initWorld
    runWith w $ do
        initialize
        -- consBrick 0
        loadLevel $ resourcePath <> "/tiled/maps/untitled.tmx"
        play display black 60 draw handleEvent $ step . float2Double