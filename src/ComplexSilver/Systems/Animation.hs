module ComplexSilver.Systems.Animation where
import Apecs
import ComplexSilver.World
import ComplexSilver.Sprites
stepAnimation :: Double -> SystemT World IO ()
stepAnimation dt = cmap $ \anim->anim {
    animTime  = animTime anim + animSpeed anim * dt
,   animState = floor (animTime anim) `mod` length (animReel anim)
}
