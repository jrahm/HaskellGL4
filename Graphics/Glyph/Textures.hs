module Graphics.Glyph.Textures where

import Data.Array.Storable
import Data.Word

import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL
import Control.Monad

data Pixels =
    PixelsRGB  (Int,Int) (StorableArray Int Word8) |
    PixelsRGBA (Int,Int) (StorableArray Int Word8)

pixelsArray :: Pixels -> StorableArray Int Word8
pixelsArray (PixelsRGB  _ a) = a
pixelsArray (PixelsRGBA _ a) = a
-- construct a new 2d array of pixels
makePixelsRGB :: (Int, Int) -> IO Pixels
makePixelsRGB a@(w,h) = liftM (PixelsRGB a) (newArray_ (0,w*h-1))

-- convert a list of rgb values to an array
newPixelsFromListRGB :: (Int, Int) -> [(Word8,Word8,Word8)] -> IO Pixels
newPixelsFromListRGB a@(w,h) lst = liftM (PixelsRGB a) $ (newListArray (0,w*h*3) .
                                                    concatMap (\(x,y,z)->[x,y,z])) lst

newPixelsFromListRGBA :: (Int, Int) -> [(Word8,Word8,Word8,Word8)] -> IO Pixels
newPixelsFromListRGBA a@(w,h) lst = liftM (PixelsRGBA a) $ newListArray (0,w*h*4)
                                                    (concatMap (\(x,y,z,q)->[x,y,z,q]) lst)

attachPixelsToTexture :: Pixels -> TextureObject -> IO ()
attachPixelsToTexture pixels tex =
    withStorableArray (pixelsArray pixels) $ \ptr -> do
        textureBinding Texture2D $= Just tex
        case pixels of
            PixelsRGB  (w,h) _ -> glTexImage2D gl_TEXTURE_2D 0 3 (f w) (f h) 0 gl_RGB gl_UNSIGNED_BYTE ptr
            PixelsRGBA (w,h) _ -> glTexImage2D gl_TEXTURE_2D 0 4 (f w) (f h) 0 gl_RGBA gl_UNSIGNED_BYTE ptr
        where f = fromIntegral


