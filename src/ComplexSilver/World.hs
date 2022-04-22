{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module ComplexSilver.World where
import Apecs
import Apecs.Physics
import Apecs.Physics.Gloss
import ComplexSilver.Sprites


makeWorld "World" [''Camera, ''Animation, ''Physics, ''SpriteSheets]