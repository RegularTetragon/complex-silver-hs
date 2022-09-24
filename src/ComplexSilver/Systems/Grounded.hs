{-# LANGUAGE ScopedTypeVariables #-}
module ComplexSilver.Systems.Grounded where
import ComplexSilver.World
import Apecs.Physics
import ComplexSilver.Components.Player (Grounded(Grounded))
import Control.Monad (unless)

xCoord (V2 x y) = x
yCoord (V2 x y) = y

bounds :: Double -> [WVec] -> (WVec, WVec)
bounds radius verts = (
        V2 (lowX-radius) (lowY-radius),
        V2 (highX+radius) (highY+radius)
    )
    where
        xs = xCoord <$> verts
        ys = yCoord <$> verts
        lowX = minimum xs
        highX = maximum xs
        lowY = minimum ys
        highY = maximum ys

groundedCollisionCallback :: Entity -> Collision -> SystemT World IO ()
groundedCollisionCallback e col = do
    Grounded alreadyGrounded <- get e
    unless alreadyGrounded $ do
        isGrounded <- checkGrounded col
        e $= Grounded isGrounded
    
groundEpsilon :: Double
groundEpsilon = 3

groundPadding :: Double
groundPadding = 2

isBetween leftBound rightBound test = test > leftBound && test < rightBound

checkGrounded :: Collision -> SystemT World IO Bool
checkGrounded (Collision norm primaryBodyEntity groundBodyEntity primaryShapeEntity groundShapeEntity) = do
    groundBody :: Body <- get groundBodyEntity
    if groundBody == StaticBody then do
        (Shape entityA (Convex vertsA radA)) <- get primaryShapeEntity
        (Shape entityB (Convex vertsB radB)) <- get groundShapeEntity
        Position posA <- get primaryBodyEntity
        Position posB <- get groundBodyEntity
        liftIO $ print $ show entityA <> "," <> show entityB
        -- Extract Lower Bounds and Upper Bounds
        let (V2 aLeft aDown, V2 aRight aUp) = bounds radA $ (+posA) <$> vertsA
        let (V2 bLeft bDown, V2 bRight bUp) = bounds radB $ (+posB) <$> vertsB
        let alignedX = isBetween bLeft bRight
        liftIO $ print (aDown - bUp)
        return $ (alignedX (aLeft+groundPadding) || alignedX (aRight-groundPadding)) 
                && abs (aDown - bUp) < groundEpsilon
    else
        return False
    

stepGrounded :: Double -> SystemT World IO ()
stepGrounded deltaTime = cmap $ \(_::Grounded) -> Grounded False