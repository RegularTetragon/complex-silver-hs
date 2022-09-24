module ComplexSilver.Systems.TerminalVelocity where
import Apecs.Physics
import Apecs
import ComplexSilver.World
import ComplexSilver.Components.TerminalVelocity
import Control.Monad.IO.Class (MonadIO)

stepTerminal :: MonadIO m => Double -> SystemT World m ()
stepTerminal deltaTime = cmap updateForce
    where
        updateForce (TerminalVelocity terminal, Velocity (V2 x y)) = Velocity (V2 x (max y terminal) )