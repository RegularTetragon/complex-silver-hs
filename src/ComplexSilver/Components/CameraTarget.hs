{-# LANGUAGE TypeFamilies #-}
module ComplexSilver.Components.CameraTarget where
import Apecs
import Apecs.Gloss (Camera(Camera))
import Apecs.Physics
import ComplexSilver.Math (vecDouble2Float)

newtype CameraTarget = CameraTarget Int
instance Component CameraTarget where
    type Storage CameraTarget = Map CameraTarget