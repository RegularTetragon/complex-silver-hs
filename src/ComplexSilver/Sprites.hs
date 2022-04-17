module ComplexSilver.Sprites where

import qualified Data.ByteString as BS
import Codec.Picture.Repa
import Codec.Picture.Types (DynamicImage (ImageRGBA8), Image (imageWidth, imageHeight, imageData))
import Graphics.Gloss.Data.Bitmap
import Apecs.Gloss
errorPicture = Line ([(-8,-8),(8,8)]) <> Line ([(-8,8), (8, -8)])
--https://stackoverflow.com/questions/12222728/png-to-bmp-in-haskell-for-gloss
readPng :: FilePath -> Int -> Int -> IO Picture
readPng path w h = readImageRGBA path >>= handle
  where
    handle (Right img) = do
      let bs = toByteString $ reverseColorChannel img
      return $ bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) bs True
    handle (Left str) = do
      print str
      return $ errorPicture

readSpritemap :: FilePath -> Int -> Int -> Int -> Int -> IO [Picture]
readSpritemap path w h tileW tileH = do
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