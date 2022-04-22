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
import ComplexSilver.Sprites (readSpritemap, Animation (..))
import ComplexSilver.World
import ComplexSilver.Level

import System.Exit
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)
import GHC.Float
import ComplexSilver.Systems.Animation

collisionFilter = CollisionFilter 1 maskAll maskAll
material = (Friction 0.4, Elasticity 0.8, Density 1)

mkPlayer :: [Picture] -> WVec -> System World Entity
mkPlayer sprites position = do
    player <- newEntity
        (
            DynamicBody,
            Position position,
            Animation {
                animReel = (sprites !!) <$> [1,2],
                animSpeed = 4,
                animTime = 0,
                animState = 0
            }
        )
    rigidbody <- newEntity (Shape player $ zRectangle 32)
    return player

triangle = Line [(0,16), (-16, -16), (16, -16), (0,16)]
square = Line[(-16,-16), (16,-16), (16,16), (-16,16), (-16,-16)]

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate (double2Float x) (double2Float y)

draw :: System World Picture
draw = do
    animations <- foldDraw $
        \(animation :: Animation, pos :: Position) ->
            color white . translate' pos . scale 1 1 $ animReel animation !! animState animation
    cmap $ \(_::Shape) -> material
    return $ animations

step :: Double -> System World ()
step dT = do
    stepAnimation dT
    stepPhysics dT


handleEvent :: Event -> System World ()
handleEvent event = return ()


initialize :: [Picture] -> System World ()
initialize spr = do
    set global earthGravity
    mkPlayer spr 0
    return ()

display = InWindow "Complex-Silver" (1280, 720) (10, 10)

someFunc :: IO ()
someFunc = do
    w <- initWorld
    sprites <- readSpritemap "/home/vince/Desktop/complex-silver/resources/sprites/sprites.png" 128 128 8 8
    runWith w $ do
        level <- liftIO $ loadLevel "~"
        initialize $ scale 8 8 <$> sprites
        play display black 60 draw handleEvent $ step . float2Double