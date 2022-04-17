{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module ComplexSilver.World where
import Apecs
import Apecs.Physics
import Apecs.Physics.Gloss

data Animation = Animation {
    animReel     :: [Picture]
,   animSpeed    :: Double
,   animTime     :: Double
,   animState    :: Int
}
instance Component Animation where
    type Storage Animation = Map Animation

makeWorld "World" [''Camera, ''Animation, ''Physics]
