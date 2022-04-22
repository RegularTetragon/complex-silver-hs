module ComplexSilver.Systems.Spritesheet where
import Apecs
import ComplexSilver.World
import ComplexSilver.Sprites (SpriteSheets(SpriteSheets), readSpritemap)
import Apecs.Gloss (Picture)

fromMaybeOrM :: Monad m => Maybe a -> m a -> m a
fromMaybeOrM Nothing action = action
fromMaybeOrM (Just value) _ = return value

loadSprite :: String -> Int -> System World Picture
loadSprite filePath id = do
    SpriteSheets sheets <- get global
    let maybeSprites = lookup filePath sheets
    sprites <- fromMaybeOrM maybeSprites $ do
        pictures <- lift $ readSpritemap filePath 256 256 8 8
        SpriteSheets existing <- get global 
        set global $ SpriteSheets $ (filePath,pictures):existing
        return $ pictures
    return $sprites !! id