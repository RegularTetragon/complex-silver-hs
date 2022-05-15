{-# LANGUAGE ScopedTypeVariables #-}
module ComplexSilver.Systems.Player where
import Apecs
import ComplexSilver.World
import Apecs.Physics
import ComplexSilver.Components.Player

stepPlayer :: Double -> SystemT World IO ()
stepPlayer deltaTime = do
    cmapM $ \(Velocity velocity, movement::PlayerMovement)->do
        return $ Velocity $ velocity
            + if movingLeft movement then V2 (-8) 0 else 0
            + if movingRight movement then V2 8 0 else 0
            + if jumping movement then V2 0 (30) else 0