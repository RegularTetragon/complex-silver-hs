{-# LANGUAGE ScopedTypeVariables #-}
module ComplexSilver.Systems.Player where
import Apecs
import ComplexSilver.World
import Apecs.Physics
import ComplexSilver.Components.Player

playerWalkMax = 128 :: Double
playerAccel = 512 :: Double
playerJumpSpeed = 200 :: Double

updatePlayerMovement :: Monad m => Double -> (BodyMass, Velocity, PlayerMovement, Grounded) -> m (Force, PlayerMovement)
updatePlayerMovement deltaTime (BodyMass mass, Velocity (V2 x y), pm@(PlayerMovement movingLeft movingRight jumping jumpTimer), Grounded isGrounded) = do
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