{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BitmapFont 
  ( BitmapFont(..)
  ) where 

import Control.Monad (forM_)

import Drawing (MonadDraw(..), Texture(..), Rect(..), Font(..), Color(..))

data BitmapFont t = BitmapFont
  { bfTexture      :: t
  , bfBitmapWidth  :: Int
  , bfBitmapHeight :: Int
  , bfBaseX        :: Int
  , bfBaseY        :: Int
  , bfSkipX        :: Int
  , bfSkipY        :: Int
  , bfCharWidth    :: Int 
  , bfCharHeight   :: Int
  , bfRowSize      :: Int
  , bfBaseChars    :: Char
  }

bfCharRect :: BitmapFont m -> Char -> Rect
bfCharRect font c = 
    Rect x y width height
  where
    i = fromEnum c - fromEnum (bfBaseChars font)
    col = mod i (bfRowSize font)
    row = div i (bfRowSize font)
    x = fromIntegral (col * bfSkipX font + bfBaseX font) / fromIntegral (bfBitmapWidth font)
    y = fromIntegral (row * bfSkipY font + bfBaseY font) / fromIntegral (bfBitmapHeight font)
    width  = fromIntegral (bfCharWidth font) / fromIntegral (bfBitmapWidth font)
    height = fromIntegral (bfCharHeight font) / fromIntegral (bfBitmapHeight font)

instance Texture m t => Font m (BitmapFont t) where

    textWidth font text size = 
        return $ size * fromIntegral (length text * bfCharWidth font) / fromIntegral (bfCharHeight font)

    drawText font text x y size (Color r g b a) = do
        forM_ (zip [0..] text) $ \(i, char) -> do
            drawTexture (bfTexture font) (position i) (bfCharRect font char)
      where 
        dx = size * fromIntegral (bfCharWidth font) / fromIntegral (bfCharHeight font) 
        position :: Int -> Rect
        position i = Rect (x + dx * fromIntegral i) y dx size

-- drawText font text x y size (Color r g b a) = do
--         context <- lift ask
--         liftIO $ setColor context (Color r g b)
--         -- lift $ bindTexture (glRenderingContext context) gl_TEXTURE_2D (bfTexture font)
--         -- lift $ blendFunc (glRenderingContext context) gl_SRC_COLOR gl_ONE_MINUS_SRC_COLOR
--         forM_ (zip [0..] text) $ \(i, c) -> do
--             let Rect x' y' width' height'     = Rect (x + dx * fromIntegral i) y dx size
--                 Rect x'' y'' width'' height'' = bfCharRect font c
--             liftIO $ drawArray context 
--                 gl_TRIANGLE_STRIP
--                 [x' + width', y' + height', x'' + width'', y''           , r, g, b, 1, 0, 0, 1, 0,
--                  x',          y' + height', x'',           y''           , r, g, b, 1, 0, 0, 1, 0,
--                  x' + width', y'          , x'' + width'', y'' + height'', r, g, b, 1, 0, 0, 1, 0,
--                  x',          y'          , x'',           y'' + height'', r, g, b, 1, 0, 0, 1, 0]
--             -- drawVertex (x' + width', y' + height') (x'' + width'', y''           )  (Color r g b) 1
--             -- drawVertex (x',          y' + height') (x'',           y''           )  (Color r g b) 1
--             -- drawVertex (x' + width', y'          ) (x'' + width'', y'' + height'')  (Color r g b) 1
--             -- drawVertex (x',          y'          ) (x'',           y'' + height'')  (Color r g b) 1
--             -- --
--             -- drawVertex (x' + width', y'          ) (x'' + width'', y'' + height'')  (Color r g b) 1
--             -- drawVertex (x',          y' + height') (x'',           y''           )  (Color r g b) 1
--         liftIO $ setColor context (Color 1 1 1)
--       where 
--         dx = size * fromIntegral (bfCharWidth font) / fromIntegral (bfCharHeight font) 



