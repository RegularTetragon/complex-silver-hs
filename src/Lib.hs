{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Lib
    ( someFunc
    ) where
import Apecs
import Apecs.Physics
import Apecs.Physics.Gloss
import Linear (V2(..))

import System.Exit
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)
import GHC.Float

data Animation = Animation {
    animReel     :: [Picture]
,   animSpeed    :: Double
,   animTime     :: Double
,   animState    :: Int
}
material = (Friction 0.4, Elasticity 0.8, Density 1)
collisionFilter = CollisionFilter 1 maskAll maskAll

makeWorld "World" [''Camera, ''Animation, ''Physics]


instance Component Animation where type Storage Animation = Map Animation


stepAnimation :: Double -> System World ()
stepAnimation dt = cmap $ \(anim :: Animation)->anim {
    animTime  = animTime anim + animSpeed anim * dt
,   animState = (floor $ animTime anim) `mod` (length $ animReel anim)
}

mkPlayer :: WVec -> System World Entity
mkPlayer position = do
    player <- newEntity
        (
            DynamicBody,
            Position position,
            Animation {
                animReel = [square, triangle],
                animSpeed = 1,
                animTime = 0,
                animState = 0
            }
        )
    rigidbody <- newEntity (Shape player $ zRectangle 32)
    return player

triangle = Line [(0,0.5), (-0.5, -0.5), (0.5, -0.5), (0,0.5)]
square = Line[(-0.5,-0.5), (0.5,-0.5), (0.5,0.5), (-0.5,0.5), (-0.5,-0.5)]

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate (double2Float x) (double2Float y)

draw :: System World Picture
draw = do
    animations <- foldDraw $
        \(animation :: Animation, pos :: Position) ->
            color white . translate' pos . scale 32 32 $ animReel animation !! animState animation
    cmap $ \(_::Shape) -> material
    return $ animations

step :: Double -> System World ()
step dT = do
    stepAnimation dT
    stepPhysics dT


handleEvent :: Event -> System World ()
handleEvent event = return ()


initialize :: System World ()
initialize = do
    set global (earthGravity)
    mkPlayer 0
    return ()

display = InWindow "Complex-Silver" (1280, 720) (10, 10)

someFunc :: IO ()
someFunc = do
    w <- initWorld
    runWith w $ do
        initialize
        play display black 60 draw handleEvent $ step . float2Double