{-# LANGUAGE TypeFamilies #-}
module ComplexSilver.Sprites where

import qualified Data.ByteString as BS
import Codec.Picture.Repa
import Codec.Picture.Types (DynamicImage (ImageRGBA8), Image (imageWidth, imageHeight, imageData))
import Graphics.Gloss.Data.Bitmap
import Apecs.Gloss
import Apecs
errorPicture = Line [(-8,-8),(8,8)] <> Line [(-8,8), (8, -8)]
--https://stackoverflow.com/questions/12222728/png-to-bmp-in-haskell-for-gloss
readPng :: FilePath -> Int -> Int -> IO Picture
readPng path w h = readImageRGBA path >>= handle
  where
    handle (Right img) = do
      let bs = toByteString $ reverseColorChannel img
      return $ bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) bs True
    handle (Left str) = do
      print str
      return errorPicture

readSpritemap :: FilePath -> Int -> Int -> Int -> Int -> IO [Picture]
readSpritemap path w h tileW tileH = do
  print path
  (Bitmap bmpData) <- readPng path w h
  return $ do
    y <- [0..h `div` tileH]
    x <- [0..w `div` tileW]
    return $
      BitmapSection (
        Rectangle {
          rectPos = (x * tileW, y * tileH),
          rectSize = (tileW, tileH)
        }
      ) bmpData

data Animation = Animation {
    animReel     :: [Picture]
,   animSpeed    :: Double
,   animTime     :: Double
,   animState    :: Int
,   animFlipX    :: Bool
,   animFlipY    :: Bool
}
defaultAnimation = Animation {
  animReel = [],
  animSpeed = 0,
  animTime = 0,
  animState = 0,
  animFlipX = False,
  animFlipY = False
}
instance Component Animation where
    type Storage Animation = Map Animation


newtype SpriteSheets = SpriteSheets [(String, [Picture])]
instance Component SpriteSheets where
    type Storage SpriteSheets = Map SpriteSheets