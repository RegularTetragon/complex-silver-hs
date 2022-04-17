module ComplexSilver.Level where
import Apecs
import ComplexSilver.World
import Data.Tiled.Load
loadLevel :: FilePath -> IO (System World ())
loadLevel filePath = do
    tiledMap <- loadMapFile filePath
    