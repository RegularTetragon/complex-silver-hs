module ComplexSilver.Math where
import GHC.Float (double2Float, float2Double)
import Apecs.Physics
vecDouble2Float :: V2 Double -> V2 Float
vecDouble2Float (V2 x y) = V2 (double2Float x) (double2Float y)
vecFloat2Double :: V2 Float -> V2 Double
vecFloat2Double (V2 x y) = V2 (float2Double x) (float2Double y)
