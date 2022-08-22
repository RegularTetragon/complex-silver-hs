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
        liftIO $ print  isGrounded
        e $= Grounded isGrounded
    
groundEpsilon :: Double
groundEpsilon = 3

checkGrounded ::Collision -> SystemT World IO Bool
checkGrounded (Collision norm primaryBodyEntity groundBodyEntity primaryShapeEntity groundShapeEntity) = do
    groundBody :: Body <- get groundBodyEntity
    if isStatic groundBody then do
        
        (Shape radiusA (Convex vertsA radA)) <- get primaryShapeEntity
        (Shape radiusB (Convex vertsB radB)) <- get groundShapeEntity
        Position posA <- get primaryBodyEntity
        Position posB <- get groundBodyEntity
        
        -- Extract Lower Bounds and Upper Bounds
        let (V2 aLeft aDown, V2 aRight aUp) = bounds radA $ (+posA) <$> vertsA
        let (V2 bLeft bDown, V2 bRight bUp) = bounds radB $ (+posB) <$> vertsB
        liftIO $ print $ aDown - bUp
        return $ abs (aDown - bUp) < groundEpsilon
    else
        return False
    where
        isStatic :: Body -> Bool
        isStatic = (==StaticBody)

stepGrounded :: Double -> SystemT World IO ()
stepGrounded deltaTime = cmap $ \(_::Grounded) -> Grounded False