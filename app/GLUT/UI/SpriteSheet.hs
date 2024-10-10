{-# LANGUAGE LambdaCase #-}

module UI.SpriteSheet
  ( SpriteSheet
  , SpriteName(..)
  , TextureBox(..)
  , Open(..)
  , Wiggle(..)
  , BlinkColor(..)
  , drawSprite
  , drawSprite'
  , loadSheetFromPng
  ) 
  where

import Graphics.UI.GLUT
import Codec.Picture

import Data.Vector.Storable (unsafeWith)

import Game.Enums
import Data.Ix
import Codec.Picture.Png.Internal.Type (PngIHdr(width, height))

data Open = OPEN | HALF_OPEN | CLOSED
    deriving (Eq, Show, Ord, Enum, Bounded, Ix)

data Wiggle  = WIGGLE_1 | WIGGLE_2
    deriving (Eq, Show, Ord, Enum, Bounded, Ix)

data BlinkColor = LIGHT | DARK
    deriving (Eq, Show, Ord, Enum, Bounded, Ix)

data SpriteName 
    = SpriteItem Item 
    | SpritePacman Direction Open
    | SpriteGhost GhostName Direction Wiggle
    | SpriteRetreating Direction
    | SpriteFrightened BlinkColor Wiggle
    | SpriteLife 
    | SpriteMuted Bool
    deriving (Eq, Show, Ord)


class Texture a where
    texWidth     :: a -> Int 
    texHeight    :: a -> Int
    texLoadImage :: String -> IO a

data TextureBox = TextureBox
  { boxX      :: !Float
  , boxY      :: !Float
  , boxWidth  :: !Float
  , boxHeight :: !Float
  , boxScale  :: !Float
  }

data SpriteSheet = SpriteSheet
  { sheetTexture :: !TextureObject
  , sheetWidth   :: !Float
  , sheetHeight  :: !Float
  -- , sheetSprites :: !Map SpriteName Sprite
  }

drawSprite' :: SpriteSheet -> TextureBox -> (Float, Float) -> (Float, Float) -> IO ()
drawSprite' sheet (TextureBox tX tY tWidth tHeight tScale) (pX, pY) (pWidth, pHeight) = 
     drawSprite 
        sheet 
        (pX + pOffsetX, pY + pOffsetY) 
        (pWidth - 2 * pOffsetX, pHeight - 2 * pOffsetY) 
        ((tX, tY), (tX + tWidth, tY + tHeight))
  where  
    pOffsetX = 0.5 * (1 - tScale) * pWidth
    pOffsetY = 0.5 * (1 - tScale) * pHeight

drawSprite :: SpriteSheet -> (Float, Float) -> (Float, Float) -> ((Float, Float), (Float, Float)) -> IO ()
drawSprite sheet (x, y) (width, height) ((u1, v1), (u2, v2)) = do
    texture Texture2D $= Enabled
    textureBinding Texture2D $= Just (sheetTexture sheet)
    blend      $= Enabled
    blendFunc  $= (SrcAlpha, OneMinusSrcAlpha)
    color (Color3 1 1 1 :: Color3 Float) -- TODO
    let scaleU = 1 / sheetWidth sheet
        scaleV = 1 / sheetHeight sheet
    renderPrimitive Quads $ do
        texCoord $ TexCoord2 (u1 * scaleU) (v2 * scaleV)
        vertex $ Vertex2 x y
        texCoord $ TexCoord2 (u1 * scaleU) (v1 * scaleV)
        vertex $ Vertex2 x (y + height)
        texCoord $ TexCoord2 (u2 * scaleU) (v1 * scaleV)
        vertex $ Vertex2 (x + width) (y + height)
        texCoord $ TexCoord2 (u2 * scaleU) (v2 * scaleV)
        vertex $ Vertex2 (x + width) y
    blend $= Disabled
    texture Texture2D $= Disabled

textureImageRGBA8 :: Image PixelRGBA8 -> IO ()
textureImageRGBA8 image =
    let width  = fromIntegral $ Codec.Picture.imageWidth image
        height = fromIntegral $ Codec.Picture.imageHeight image
        size   = TextureSize2D width height
    in do
        unsafeWith (imageData image) $
            texImage2D Texture2D NoProxy 0 RGBA8 size 0 . PixelData RGBA UnsignedByte -- glTexImage2D
        generateMipmap' Texture2D

loadSheetFromPng :: FilePath -> IO SpriteSheet
loadSheetFromPng filename = 
    Codec.Picture.readPng filename >>= \case
        Left err -> fail err
        Right image -> do 
            let image' = convertRGBA8 image
            tex <- genObjectName :: IO TextureObject
            texture        Texture2D $= Enabled
            textureBinding Texture2D $= Just tex
            textureFilter  Texture2D $= ((Nearest, Nothing), Nearest)
            textureImageRGBA8 image'
            texture Texture2D $= Disabled
            let width  = fromIntegral $ Codec.Picture.imageWidth image'
                height = fromIntegral $ Codec.Picture.imageHeight image'
            return $ SpriteSheet tex width height