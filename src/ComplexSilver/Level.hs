{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module ComplexSilver.Level where
import ComplexSilver.Constructors.MapConstructors
import Apecs
import ComplexSilver.World
import Data.Tiled
import Data.Maybe
import Data.Map
import Apecs.Physics

loadLevel :: FilePath -> SystemT World IO [Entity]
loadLevel filePath = do
    tiledMap <- lift $ loadMapFile filePath
    let TiledMap {
        mapTilesets = tilesets,
        mapLayers = layers
    } = tiledMap
    sequence $ catMaybes $ do
        layer <- layers
        ((x,y), tile) <- toList $ layerData layer
        return $ consFromTileId (tileGid tile) (8 * V2 (fromIntegral x) (negate $ fromIntegral y))