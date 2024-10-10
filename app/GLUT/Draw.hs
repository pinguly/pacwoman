{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Draw 
  ( MonadDraw (..)
  , loadSheetFromPng
  , drawLifes
  , drawMute
  ) 
  where

import Control.Monad (forM_, when)
import Control.Monad.State
import qualified Data.Array as Array
import qualified Data.Map as Map

import Animation
-- import UI.SpriteSheet
import qualified UI.Font as Font

import Graphics.UI.GLUT hiding (get, Color)
import Codec.Picture

import Data.Vector.Storable (unsafeWith)

import Game.Enums
import Game.Map.Chart
import Game.Pacman
import Game.Ghost
import Game.State 
import Game.Entity (isLeavingCell, Entity (lensDirection))
import Control.Lens (view)


import Drawing

fmod :: Float -> Float -> Float
fmod a b = a - b * fromIntegral (floor (a / b))

instance MonadDraw IO where

    type Texture IO = TextureObject

    drawRect (Rect x y width height) (Color r g b a) = liftIO $ do
        color $ Color4 r g b a
        renderPrimitive Quads $ do
            -- texCoord (TexCoord2 0 1 :: TexCoord2 Float)
            vertex $ Vertex2 x y
            -- texCoord (TexCoord2 0 0 :: TexCoord2 Float)
            vertex $ Vertex2 x (y + height)
            -- texCoord (TexCoord2 1 0 :: TexCoord2 Float)
            vertex $ Vertex2 (x + width) (y + height)
            -- texCoord (TexCoord2 1 1 :: TexCoord2 Float)
            vertex $ Vertex2 (x + width) y

    drawTexture tex (Rect x y width height) (Rect u1 v1 width' height') = do
        texture Texture2D $= Enabled
        textureBinding Texture2D $= Just tex
        blend      $= Enabled
        blendFunc  $= (SrcAlpha, OneMinusSrcAlpha)
        color (Color3 1 1 1 :: Color3 Float) -- TODO
        let u2 = u1 + width'
            v2 = v1 + height'
        renderPrimitive Quads $ do
            texCoord $ TexCoord2 u1 v2
            vertex $ Vertex2 x y
            texCoord $ TexCoord2 u1 v1
            vertex $ Vertex2 x (y + height)
            texCoord $ TexCoord2 u2 v1
            vertex $ Vertex2 (x + width) (y + height)
            texCoord $ TexCoord2 u2 v2
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

loadSheetFromPng :: FilePath -> IO (SpriteSheet IO)
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
            putStrLn $ show (width, height)
            return $ SpriteSheet tex width height

-- drawPacman :: Pacman -> LevelPhase -> m ()
--drawPacman pacman phase = do
--    sheet <- get
--    liftIO $ drawSprite' 
--        sheet 
--        sprite
--        (pacpos_x + 0.5 * cellsize * cx, pacpos_y + 0.5 * cellsize * cy) 
--        (cellsize, cellsize) 
--  where 
--    cellsize = cellSize (pacCell pacman)
--    (pacpos_x, pacpos_y) = cellOrigin (pacCell pacman)
--    (cx, cy)   = pacCenter pacman
--    sprite = case phase of 
--        STARTING  -> pacSpriteFull pacman 
--        RESPAWN   -> pacSpriteFull pacman 
--        WINNING _ -> pacSpriteFull pacman
--        DYING i   -> pacSpriteDying pacman i
--        _         -> pacSpriteNormal pacman

-- drawItems :: Map.Map Cell Item -> Animations -> m ()
--drawItems items anims = do
--    sheet <- get
--    forM_ (Map.assocs items) $ \(cell, item) -> do
--        let cellsize = cellSize cell
--            (x, y)   = cellOrigin cell
--            sprite = case item of 
--                        POINT   -> TextureBox 2 182 8 8 0.3
--                        REVERSE -> if blinkOn $ animDotBlink anims 
--                                        then TextureBox 2 182 8 8 0.8
--                                        else TextureBox 40 20 20 20 0.8
--                        FRUIT   -> TextureBox 172 164 12 13 1
--        liftIO $ drawSprite' 
--            sheet 
--            sprite
--            (x, y) 
--            (cellsize, cellsize)

-- drawGhost :: Ghost -> GhostPhase -> m ()
--drawGhost ghost phase = do
--    liftIO $ color (Color3 1 1 1 :: Color3 Float)
--    sheet <- get
--    liftIO $ drawSprite' 
--        sheet
--        sprite
--        (ghostpos_x + 0.5 * cellsize * ghostc_x, ghostpos_y + 0.5 * cellsize * ghostc_y) 
--        (cellsize, cellsize) 
--  where 
--    cellsize = cellSize (ghostCell ghost)
--    (ghostpos_x, ghostpos_y) = cellOrigin (ghostCell ghost)
--    (ghostc_x, ghostc_y) = ghostCenter ghost
--    sprite = ghostSprite ghost phase


-- drawLifes :: SpriteSheet IO -> (Float, Float) -> Float -> Int -> IO ()
-- drawLifes sheet (x, y) size n =
--     preservingMatrix $ do
--         forM_ [1..n] $ \_ -> do
--             translate $ Vector3 (-1.5 * size) 0 0 -- TODO offset 
--             drawSprite' sprite (x, y) (size, size)
--   where 
--     sprite = mkTextureBox sheet 85 163 10 12 1
            

-- drawMute :: SpriteSheet IO ->  (Float, Float) -> Float -> Bool -> IO ()
-- drawMute sheet (x, y) size muted =
--     preservingMatrix $ 
--         drawSprite' sprite (x, y) (size, size)                
--   where 
--     sprite = if muted 
--                 then mkTextureBox sheet 234 102 14 14 1
--                 else mkTextureBox sheet 214 102 14 14 1 



--drawCircleSegment :: (Float, Float) -> Float -> Int -> (Int, Int) -> IO ()
--drawCircleSegment (x, y) radius n (start, stop) =
--    renderPrimitive TriangleFan $ do
--        vertex $ Vertex2 x y
--        forM_ [start .. stop] $ \i -> do 
--            let theta = 2 * pi * fromIntegral i / fromIntegral n
--            vertex $ Vertex2 (x + radius * cos theta) (y + radius * sin theta)


-- drawPacman :: Pacman -> Float -> SpriteSheet -> Animations -> LevelPhase -> IO ()
-- drawPacman pacman offset sheet anims phase = do
--     color (Color3 1 1 0 :: Color3 Float)
--     let x = pacpos_x + 0.5 * cellsize * (1 + cx)
--         y = pacpos_y + 0.5 * cellsize * (1 + cy)
--         r = 0.5 * cellsize + offset
--         start = case pacDirection pacman of 
--                     RIGHT -> 0
--                     UP    -> 4
--                     LEFT  -> 8
--                     DOWN  -> 12
--         angle = case phase of 
--                     STARTING  -> 0
--                     RESPAWN   -> 0 
--                     WINNING _ -> 0
--                     DYING i   -> 2 + div i 2
--                     _         -> 
--                         if blinkOn (animPacBlink anims) || pacCenter pacman == (0, 0)
--                             then 1 
--                             else 2
--     drawCircleSegment (x, y) r 16 (start + angle, 16 + start - angle)
--   where 
--     cellsize = cellSize (pacCell pacman)
--     (pacpos_x, pacpos_y) = cellOrigin (pacCell pacman)
--     (cx, cy)   = pacCenter pacman


--pacSpriteNormal :: Pacman -> TextureBox
--pacSpriteNormal pacman = 
--    TextureBox (81 + x) (1 + y) 17 17 1.7
--  where 
--    direction = view lensDirection pacman
--    x = if not (isLeavingCell pacman)
--            then 0
--            else 20
--    y = case direction of
--            LEFT  -> 0
--            RIGHT -> 20
--            UP    -> 40
--            DOWN  -> 60
--
--pacSpriteFull :: Pacman -> TextureBox
--pacSpriteFull pacman = 
--    TextureBox 121 (1 + y) 17 17 1.7
--  where 
--    y = case view lensDirection pacman of
--            LEFT  -> 0
--            RIGHT -> 20
--            UP    -> 40
--            DOWN  -> 60
--
--pacSpriteDying :: Pacman -> Int -> TextureBox
--pacSpriteDying _ i = TextureBox (1 + 20 * fromIntegral i) 239 17 17 1.7
--
--
--
--
--ghostSprite :: Ghost -> GhostPhase -> TextureBox
--ghostSprite ghost phase = 
--    TextureBox (1 + x) (1 + y) 17 17 1.7
--  where 
--    name = ghostId ghost
--    mode = ghostMode ghost
--    direction = ghostDirection ghost 
--    retreating = ghostRetreating ghost > 0
--    blink = isLeavingCell ghost -- blinkOn $ animGhostBlink anims
--    colorOffset
--        | retreating       = 10 
--        | mode == FRIGHTEN = 8
--        | otherwise = 
--            case name of 
--                INKY   -> 6
--                BLINKY -> 4
--                PINKY  -> 5
--                CLYDE  -> 7
--    dirOffset = 
--        if mode == FRIGHTEN
--            then if phaseDuration phase < 3 && fmod (phaseDuration phase) 0.8 < 0.4
--                    then 1 
--                    else 0
--        else case direction of 
--                LEFT  -> 2
--                RIGHT -> 3
--                UP    -> 0
--                DOWN  -> 1
--    blinkOffset = if blink then 0 else 1
--    x = 20 * if retreating 
--                then dirOffset
--                else 2 * dirOffset + blinkOffset
--    y = 20 * colorOffset


                    

-- drawScore :: (Float, Float) -> Float -> Int -> Font.Font -> IO ()
-- drawScore (x, y) size score font = 
--     preservingMatrix $ do 
--         translate $ Vector3 x y 0
--         scale size size 1
--         translate $ Vector3 (0 :: Float) 0.5 0 -- Für Buchstaben die runterhängen wie g
--         Font.renderText font $ "SCORE:" ++ show score
-- 
-- 




