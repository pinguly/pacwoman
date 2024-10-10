module UI.Font 
 ( Font
 , loadFont
 , renderText
 , textWidth
 ) where 

import Control.Monad (forM, forM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Foreign.Storable (peek)
import Foreign.Ptr (nullPtr)
import FreeType
import Graphics.GL
import Graphics.Rendering.OpenGL.GL


data Font = Font 
  { fontAtlas       :: GlyphAtlas
  , fontGlyphs      :: Map Int GlyphInfo
  } deriving (Eq, Show)

data GlyphAtlas = GlyphAtlas
  { atlasTexture :: TextureObject
  , atlasWidth   :: GLsizei
  , atlasRows    :: GLsizei
  } deriving (Eq, Show)

data GlyphInfo = GlyphInfo
  { glyphOffset   :: Int
  , glyphWidth    :: Int
  , glyphRows     :: Int
  , glyphLeft     :: Int
  , glyphTop      :: Int
  , glyphAdvanceX :: Float
  , glyphAdvanceY :: Float
  } deriving (Eq, Show)


loadGlyphs :: FT_Face -> [Int] -> IO [GlyphInfo]
loadGlyphs face charCodes = do
    records <- forM charCodes $ \code -> do
                   ft_Load_Char face (fromIntegral code) FT_LOAD_DEFAULT
                   peek face >>= peek . frGlyph
    
    let offsets = init . scanl (+) 0 . map (bWidth . gsrBitmap) $ records

    forM (zip offsets records) $ \(offset, record) -> do
        return GlyphInfo 
            { glyphOffset   = fromIntegral offset
            , glyphWidth    = fromIntegral . bWidth $ gsrBitmap record
            , glyphRows     = fromIntegral . bRows  $ gsrBitmap record
            , glyphLeft     = fromIntegral $ gsrBitmap_left record
            , glyphTop      = fromIntegral $ gsrBitmap_top  record
            , glyphAdvanceX = (/ 64) . fromIntegral . vX $ gsrAdvance record
            , glyphAdvanceY = (/ 64) . fromIntegral . vY $ gsrAdvance record
            }


loadFont :: FilePath -> [Int] -> Int -> IO Font
loadFont filename charCodes pixels = do
    freeType <- ft_Init_FreeType                 -- ^ FreeType library instance 
    face     <- ft_New_Face freeType filename 0  -- ^ first font family in file
    ft_Set_Pixel_Sizes face (fromIntegral pixels) 0

    glyphs <- loadGlyphs face charCodes

    let atWidth = sum . map glyphWidth $ glyphs
        atRows  = maximum . map glyphRows $ glyphs
        atSize  = TextureSize2D (fromIntegral atWidth) (fromIntegral atRows)

    atlas <- genObjectName :: IO TextureObject
    texture         Texture2D   $= Enabled
    textureBinding  Texture2D   $= Just atlas
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
    textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  
    rowAlignment Unpack $= 1
    texImage2D Texture2D NoProxy 0 Alpha8 atSize 0 $ PixelData Alpha UnsignedByte nullPtr

    forM_ (zip charCodes glyphs) $ \(code, glyph) -> do
        ft_Load_Char face (fromIntegral code) FT_LOAD_RENDER
        record <- peek face >>= peek . frGlyph
        let buffer = bBuffer . gsrBitmap $ record
            glySize = TextureSize2D (fromIntegral $ glyphWidth glyph) (fromIntegral $ glyphRows glyph)
            glyPos  = TexturePosition2D (fromIntegral $ glyphOffset glyph) 0
        texSubImage2D Texture2D 0 glyPos glySize $ PixelData Alpha UnsignedByte buffer

    texture         Texture2D $= Disabled
    ft_Done_Face face
    ft_Done_FreeType freeType

    return Font 
        { fontAtlas  = GlyphAtlas atlas (fromIntegral atWidth) (fromIntegral atRows)
        , fontGlyphs = Map.fromList $ zip charCodes glyphs
        }


textToGlyphs :: Font -> String -> [GlyphInfo]
textToGlyphs font = mapMaybe (flip Map.lookup (fontGlyphs font) . fromEnum)

textWidth :: Font -> String -> Float
textWidth font = 
    (/ rows) . sum . map glyphAdvanceX . textToGlyphs font
  where 
    rows = fromIntegral . atlasRows $ fontAtlas font 


renderText :: Font -> String -> IO ()
renderText font text = do 
    let atlas = fontAtlas font
        atWidth = realToFrac $ atlasWidth atlas
        atRows  = realToFrac $ atlasRows  atlas

    texture         Texture2D   $= Enabled
    textureBinding  Texture2D   $= Just (atlasTexture atlas)
    blend     $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

    glPushMatrix
    scale (1 / atRows) (1 / atRows) 1

    forM_ (textToGlyphs font text) $ \glyph -> do
        let offset = glyphOffset glyph
            x1 = 0  + fromIntegral (glyphLeft  glyph) :: Float
            x2 = x1 + fromIntegral (glyphWidth glyph) :: Float
            y1 = 0  + fromIntegral (glyphTop   glyph) :: Float
            y2 = y1 - fromIntegral (glyphRows  glyph) :: Float
            s1 = (fromIntegral offset + 0.5) / atWidth  :: Float
            s2 = s1 + fromIntegral (glyphWidth glyph - 1)  / atWidth
            t1 = fromIntegral (glyphRows glyph) / atRows :: Float

        renderPrimitive Quads $ do
            texCoord $ TexCoord2 s1 t1
            vertex $ Vertex2 x1 y2
            texCoord $ TexCoord2 s1 0
            vertex $ Vertex2 x1 y1
            texCoord $ TexCoord2 s2 0
            vertex $ Vertex2 x2 y1
            texCoord $ TexCoord2 s2 t1
            vertex $ Vertex2 x2 y2

        translate $ Vector3 (glyphAdvanceX glyph) (glyphAdvanceY glyph) 0

    glPopMatrix
    blend $= Disabled
    texture Texture2D $= Disabled