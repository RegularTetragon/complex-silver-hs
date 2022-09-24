{-# LANGUAGE ScopedTypeVariables #-}
module ComplexSilver.Systems.Player where
import Apecs
import ComplexSilver.World
import Apecs.Physics
import ComplexSilver.Components.Player
import ComplexSilver.Constants

playerCollisionFilter = CollisionFilter {
    filterGroup = 0,
    filterCategories = colLayerPlayer,
    filterMask = colLayerWorld
}
playerWalkMax = 128 :: Double
playerJumpSpeed = 200 :: Double

updatePlayerMovement :: Double -> (BodyMass, Velocity, PlayerMovement, Grounded) -> System World (Force, PlayerMovement)
updatePlayerMovement deltaTime (BodyMass mass, Velocity (V2 x y), pm@(PlayerMovement movingLeft movingRight jumping jumpTimer), Grounded isGrounded) = do
    lift $ print isGrounded
    return (Force force, pm {jumpTimer = if isGrounded then 0.2 else if jumping then jumpTimer - deltaTime else 0})
    where
        force = V2 ((x'-x)/deltaTime * mass) ((y'-y)/deltaTime*mass)
        y' :: Double
        y' = if jumping && jumpTimer > 0 then playerJumpSpeed else y
        x' :: Double
        x'
          | movingLeft && not movingRight = -playerWalkMax
          | movingRight && not movingLeft = playerWalkMax
          | otherwise = x * 0.9

stepPlayer :: Double -> SystemT World IO ()
stepPlayer deltaTime = do
    cmapM $ updatePlayerMovement deltaTime