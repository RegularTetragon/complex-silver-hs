{-# LANGUAGE TypeFamilies #-}
module ComplexSilver.Components.TerminalVelocity where
import Apecs
import Apecs.Physics
newtype TerminalVelocity = TerminalVelocity Double

instance Component TerminalVelocity where
    type Storage TerminalVelocity = Map TerminalVelocity