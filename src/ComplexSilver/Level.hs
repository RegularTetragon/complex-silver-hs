{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module ComplexSilver.Level where
import Apecs
import Apecs.Physics
import ComplexSilver.World
import Data.Tiled.Load
import Data.Tiled.Types
import Data.Word (Word32)
import Data.Map (toList)
import ComplexSilver.Sprites
import ComplexSilver.Systems.Spritesheet

consFail :: WVec -> System World Entity
consFail pos = do
    entity <- newEntity ()
    return entity

consBrick :: WVec -> System World Entity
consBrick pos = do
    sprite <- loadSprite "sprites.png" 8
    entity <- newEntity (
        Position pos,
        StaticBody,
        Animation {
            animReel = [sprite],
            animSpeed = 0.0,
            animState = 0,
            animTime = 0.0
        }
        )
    
    return entity

consFromTileId :: Word32 -> WVec -> System World Entity
consFromTileId 0 = consFail
consFromTileId n = consBrick

loadLevel :: FilePath -> IO [System World Entity]
loadLevel filePath = do
    tiledMap <- loadMapFile filePath
    let TiledMap {
        mapTilesets = tilesets,
        mapLayers = layers
    } = tiledMap
    return $ do
        layer <- layers
        ((x,y), tile) <- toList $ layerData layer
        return $ consFromTileId (tileGid tile) (V2 (fromIntegral x) (fromIntegral y))