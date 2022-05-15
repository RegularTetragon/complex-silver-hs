module ComplexSilver.Systems.CameraTarget where
import Apecs
import ComplexSilver.World
import Apecs.Gloss
import Apecs.Physics
import ComplexSilver.Components.CameraTarget
import ComplexSilver.Math

stepCamera :: Double -> SystemT World IO ()
stepCamera deltaTime = do
    Camera p s <- get global
    (_, Position targetVec) <- cfold compareTargets (CameraTarget minBound, Position 0)
    let newFramePos = p + (vecDouble2Float targetVec - p) * vecDouble2Float (V2 deltaTime deltaTime) * 8
    set global $ Camera newFramePos s
    where
        compareTargets left@(CameraTarget leftPriority, Position _) right@(CameraTarget rightPriority, Position _) =
            if leftPriority > rightPriority then
                left
            else
                right