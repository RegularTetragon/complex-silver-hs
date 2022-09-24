{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module ComplexSilver.Components.Facing where
import Apecs
data Direction = Right | Left
newtype Facing = Facing Direction

instance Component Facing where
    type Storage Facing = Map Facing