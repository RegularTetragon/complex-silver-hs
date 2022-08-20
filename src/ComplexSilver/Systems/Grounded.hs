{-# LANGUAGE ScopedTypeVariables #-}
module ComplexSilver.Systems.Grounded where
import ComplexSilver.World
import Apecs.Physics
import ComplexSilver.Components.Player (Grounded(Grounded))
swapIfFalse :: Bool -> (a, a) -> (a, a)
swapIfFalse b (l, r) = if b then (l,r) else (r, l)

yCoord (V2 x y) = y


checkGrounded :: Entity -> Collision -> SystemT World IO Bool
checkGrounded entity collision = do
    groundBody :: Body <- get groundBodyEntity
    if isStatic groundBody then do
        -- Todo: Check grounded details
        Shape _ convexA <- get primaryShapeEntity
        Shape _ convexB <- get groundShapeEntity
        let lowestA = minimum $ yCoord <$> vertices convexA
        let highestB = maximum $ yCoord <$> vertices convexB
        return $ highestB - lowestA > -1
    else
        return False
    where
        collisionInExpectedOrder = collisionBodyA collision == entity
        swapMode = swapIfFalse collisionInExpectedOrder
        (primaryBodyEntity , groundBodyEntity)  = swapMode (collisionBodyA collision , collisionBodyB   collision)
        (primaryShapeEntity, groundShapeEntity) = swapMode (collisionShapeA collision, collisionShapeB  collision)
        isStatic :: Body -> Bool
        isStatic StaticBody = True
        isStatic _ = False

stepGrounded :: Double -> SystemT World IO ()
stepGrounded deltaTime = cmap $ \(_::Grounded) -> Grounded False