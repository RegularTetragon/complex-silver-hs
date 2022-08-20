{-# LANGUAGE TypeFamilies #-}
module ComplexSilver.Components.Grounded where
import Apecs
newtype Grounded = Grounded Bool

instance Component Grounded where
    type Storage Grounded = Map Grounded