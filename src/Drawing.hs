{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Drawing 
  ( Color(..)
  , Rect(..)
  , rectLeft
  , rectRight
  , rectUp
  , rectDown
  , MonadDraw(..)
  , MonadCanvas(..)
  , SpriteSheet(..)
  , Texture(..)
  , TextureBox(..)
  , Font(..)
  , drawSquare
  , drawCharts
  , drawLevel
  , drawSprite'
  , mkTextureBox
  , drawItem
  , drawItems
  , drawPacman
  , drawGhost
  , drawHeader
  , drawPreviewHeader
  , drawLifes
  , drawMute
  , levelViewport
  , headerViewport
  , fmod
  ) where

import Game.Entity
import Game.Pacman
import Game.Ghost
import Game.State
import Game.Enums
import Game.Monad
import Game.Map.Chart
import Game.Map.Config

import Control.Monad
import Control.Lens (view)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Array as Array
import Config
import Data.Char (toUpper)


data Color = Color 
  { colorR :: Float
  , colorG :: Float 
  , colorB :: Float
  , colorA :: Float
  } deriving (Eq, Show)


data Rect = Rect
  { rectX      :: Float 
  , rectY      :: Float
  , rectWidth  :: Float
  , rectHeight :: Float
  } deriving (Eq, Show)

rectLeft :: Rect -> Float
rectLeft = rectX

rectRight :: Rect -> Float
rectRight = (+) <$> rectX <*> rectWidth 

rectUp :: Rect -> Float
rectUp = (+) <$> rectY <*> rectHeight

rectDown :: Rect -> Float
rectDown = rectY


class Monad m => MonadDraw m where 

    drawRect :: Rect -> Color -> m ()


class Monad m => Texture m t where
    
    drawTexture :: t -> Rect -> Rect -> m ()


class MonadDraw m => MonadCanvas m where 

    canvasWidth  :: m Float
    canvasHeight :: m Float
    setCoordinates :: Rect -> Rect -> m ()

    clearCanvas :: Color -> m ()

-- drawPacman :: Pacman -> LevelPhase -> m ()
-- drawItems :: Map Cell Item -> Animations -> m ()
-- drawGhost :: Ghost -> GhostPhase -> m ()
-- drawLifes :: (Float, Float) -> Float -> Int -> m ()
-- drawMute :: (Float, Float) -> Float -> Bool -> m ()

setViewport :: MonadCanvas m => Rect -> Rect -> Maybe Direction -> m ()
setViewport screenRect@(Rect screenX screenY screenWidth screenHeight)
            coordRect@(Rect coordX coordY coordWidth coordHeight) 
            alignment 
    = do  
        let screenRect' = if aspect >= aspect'
                            then Rect
                                    (screenX + alignX * offsetX)
                                    screenY
                                    (screenWidth - 2 * offsetX)
                                    screenHeight
                            else Rect
                                    screenX
                                    (screenY + alignY * offsetY)
                                    screenWidth
                                    (screenHeight - 2 * offsetY)
        setCoordinates screenRect' coordRect
  where 
    aspect  = screenWidth / screenHeight
    aspect' = coordWidth  / coordHeight
    offsetX = screenHeight * (aspect - aspect') / 2
    offsetY = screenWidth  * (1 / aspect - 1 / aspect') / 2
    alignX = case alignment of 
                Just LEFT  -> 0
                Just RIGHT -> 2
                _          -> 1
    alignY = case alignment of 
                Just DOWN  -> 0
                Just UP    -> 2
                _          -> 1


headerViewport :: MonadCanvas m => m ()
headerViewport = do
    screenWidth  <- canvasWidth
    screenHeight <- canvasHeight
    let borderL = 0.06 * screenWidth
        borderR = 0.06 * screenWidth
        borderU = 0.12 * screenHeight
        reducedWidth  = screenWidth - borderL - borderR
        reducedHeight = screenHeight * 0.1
    setViewport 
        (Rect borderL (screenHeight - borderU) reducedWidth reducedHeight)
        (Rect 0 0 45 5)
        (Just DOWN)

levelViewport :: MonadCanvas m => LevelRefs -> m ()
levelViewport refs = do
    width  <- canvasWidth
    height <- canvasHeight        
    setViewport
        (reducedScreen width height)
        (bbox refs)
        (Just UP) 
  where 
    bbox refs = 
        Rect minX minY width' height'
      where 
        charts = Array.elems . gsCharts .  lvlState $ refs
        minX = minimum . map (fst . chartOrigin) $ charts
        maxX = maximum . map (fst . chartOrigin) $ charts
        minY = minimum . map (snd . chartOrigin) $ charts
        maxY = maximum . map (snd . chartOrigin) $ charts
        width'  = (maxX + 1) - minX
        height' = (maxY + 1) - minY
    
    reducedScreen width height = 
        Rect borderL borderD reducedWidth reducedHeight
      where 
        borderL = 0.06 * width 
        borderR = 0.06 * width 
        borderU = 0.15 * height
        borderD = 0.06 * height
        reducedWidth  = width  - borderL - borderR 
        reducedHeight = height - borderU - borderD

drawSquare :: MonadDraw m => (Float, Float) -> Float -> Color -> m ()
drawSquare (x, y) size color = drawRect (Rect x y size size) color


drawWalls :: MonadDraw m => Cell -> Color -> m ()
drawWalls cell color = do
    when (cellBlocked cell) $ do
        when (cellBlocked neighL && not (blockedV && blockedL)) $ -- left
            drawRect (Rect x (y + offset) (cellsize - offset) (cellsize - 2 * offset)) color
        when (cellBlocked neighR && not (blockedV && blockedR)) $ -- right
            drawRect (Rect (x + offset) (y + offset) (cellsize - offset) (cellsize - 2 * offset)) color 
        when (cellBlocked neighU && not (blockedH && blockedU)) $ -- up
            drawRect (Rect (x + offset) (y + offset) (cellsize - 2 * offset) (cellsize - offset)) color
        when (cellBlocked neighD && not (blockedH && blockedD)) $ -- down
            drawRect (Rect (x + offset) y (cellsize - 2 * offset) (cellsize - offset)) color
  where 
    cellsize = cellSize cell
    offset   = 0.35 * cellsize
    (x, y)   = cellOrigin cell
    neighL   = cellNeigh cell LEFT
    neighR   = cellNeigh cell RIGHT
    neighU   = cellNeigh cell UP
    neighD   = cellNeigh cell DOWN
    blockedH = cellBlocked (cellNeigh cell LEFT)   && cellBlocked (cellNeigh cell RIGHT)
    blockedV = cellBlocked (cellNeigh cell UP)     && cellBlocked (cellNeigh cell DOWN)
    blockedL = cellBlocked (cellNeigh neighL UP)   && cellBlocked (cellNeigh neighL DOWN)
    blockedR = cellBlocked (cellNeigh neighR UP)   && cellBlocked (cellNeigh neighR DOWN)
    blockedU = cellBlocked (cellNeigh neighU LEFT) && cellBlocked (cellNeigh neighU RIGHT)
    blockedD = cellBlocked (cellNeigh neighD LEFT) && cellBlocked (cellNeigh neighD RIGHT)


drawCharts :: MonadDraw m => Charts -> Color -> m ()
drawCharts charts wallColor = 
    forM_ charts $ \chart -> do
        drawSquare (chartOrigin chart) 1 (Color 0 0 0 1) 
        forM_ (Array.elems (chartCells chart)) $ \cell -> 
            drawWalls cell wallColor


drawLevel :: (MonadDraw m, Texture m t) => SpriteSheet t -> LevelRefs -> LevelPhase -> m ()
drawLevel sheet lvlRefs lvlphase = do

    let state = lvlState lvlRefs
        
    let wallcolor = case lvlphase of
            WINNING i ->
                if even i then Color 0 0 1 1 else Color 0.5 0.5 1 1
            _         -> Color 0 0 1 1
    drawCharts (gsCharts state) wallcolor

    let t = gsTime state
        interval = cfgDotBlink . cfgTiming $ gsConfig state
        blink = fmod t (2 * interval) <= interval
    drawItems sheet (gsItems state) blink

    case lvlphase of
        GAME_OVER -> return ()
        _       -> do
            drawPacman sheet  (gsPacman state) lvlphase

    case lvlphase of
        DYING _   -> return ()
        GAME_OVER -> return ()
        EATING    -> return ()
        _       -> do
            forM_ (gsGhosts state) $ \ghost -> 
                drawGhost sheet ghost (gsGhostPhase state)


-- data Open = OPEN | HALF_OPEN | CLOSED
--     deriving (Eq, Show, Ord, Enum, Bounded, Ix)
-- 
-- data Wiggle  = WIGGLE_1 | WIGGLE_2
--     deriving (Eq, Show, Ord, Enum, Bounded, Ix)
-- 
-- data BlinkColor = LIGHT | DARK
--     deriving (Eq, Show, Ord, Enum, Bounded, Ix)
-- 
-- data SpriteName 
--     = SpriteItem Item 
--     | SpritePacman Direction Open
--     | SpriteGhost GhostName Direction Wiggle
--     | SpriteRetreating Direction
--     | SpriteFrightened BlinkColor Wiggle
--     | SpriteLife 
--     | SpriteMuted Bool
--     deriving (Eq, Show, Ord)

data TextureBox t = TextureBox
  { boxTexture :: !t
  , boxX       :: !Float
  , boxY       :: !Float
  , boxWidth   :: !Float
  , boxHeight  :: !Float
  , boxScale   :: !Float
  }

mkTextureBox :: SpriteSheet t -> SpriteConfig -> TextureBox t
mkTextureBox sheet (SpriteConfig x y width height scale) =
    TextureBox (sheetTexture sheet) (sx * x) (sy * y) (sx * width) (sy * height) scale
  where 
    sx = 1 / sheetWidth sheet
    sy = 1 / sheetHeight sheet

data SpriteSheet t = SpriteSheet
  { sheetTexture :: !t
  , sheetWidth   :: !Float
  , sheetHeight  :: !Float
  , sheetConfig  :: !SpriteSheetConfig
  }

drawSprite' :: Texture m t => TextureBox t -> (Float, Float) -> (Float, Float) -> m ()
drawSprite' (TextureBox texture tX tY tWidth tHeight tScale) (pX, pY) (pWidth, pHeight) = 
     drawTexture 
        texture 
        (Rect (pX + pOffsetX) (pY + pOffsetY) (pWidth - 2 * pOffsetX) (pHeight - 2 * pOffsetY)) 
        (Rect (tX) (tY) tWidth tHeight)
  where  
    pOffsetX = 0.5 * (1 - tScale) * pWidth
    pOffsetY = 0.5 * (1 - tScale) * pHeight



fmod :: Float -> Float -> Float
fmod a b = a - b * fromIntegral (floor (a / b))


drawPacman :: Texture m t => SpriteSheet t -> Pacman -> LevelPhase -> m ()
drawPacman sheet pacman phase = do
    drawSprite'  
        sprite
        (pacpos_x + 0.5 * cellsize * cx, pacpos_y + 0.5 * cellsize * cy) 
        (cellsize, cellsize) 
  where 
    cellsize = cellSize (pacCell pacman)
    (pacpos_x, pacpos_y) = cellOrigin (pacCell pacman)
    (cx, cy)   = pacCenter pacman
    sprite = case phase of 
        STARTING  -> pacSpriteFull sheet pacman 
        RESPAWN   -> pacSpriteFull sheet pacman 
        WINNING _ -> pacSpriteFull sheet pacman
        DYING i   -> pacSpriteDying sheet pacman i
        _         -> pacSpriteNormal sheet pacman

drawItem :: Texture m t => SpriteSheet t -> Cell -> Item -> Bool -> m ()
drawItem sheet cell item blink = 
    when blink $
        drawSprite' sprite (x, y) (cellsize, cellsize)
  where
    cellsize = cellSize cell
    (x, y)   = cellOrigin cell
    sprite = mkTextureBox sheet $ case item of 
                POINT   -> sprtPoint $ sheetConfig sheet
                REVERSE -> sprtReverse $ sheetConfig sheet
                FRUIT   -> sprtFruit $ sheetConfig sheet
        
drawItems :: Texture m t => SpriteSheet t -> Map Cell Item -> Bool -> m ()
drawItems sheet items blink = do
    forM_ (Map.assocs items) $ \(cell, item) -> do
        drawItem sheet cell item blink

drawGhost :: Texture m t => SpriteSheet t -> Ghost -> GhostPhase -> m ()
drawGhost sheet ghost phase = do
   drawSprite' 
        sprite
        (ghostpos_x + 0.5 * cellsize * ghostc_x, ghostpos_y + 0.5 * cellsize * ghostc_y) 
        (cellsize, cellsize) 
  where 
    cellsize = cellSize (ghostCell ghost)
    (ghostpos_x, ghostpos_y) = cellOrigin (ghostCell ghost)
    (ghostc_x, ghostc_y) = ghostCenter ghost
    sprite = ghostSprite sheet ghost phase


pacSpriteNormal :: SpriteSheet t -> Pacman -> TextureBox t
pacSpriteNormal sheet pacman = 
    mkTextureBox sheet $ SpriteConfig (312 + x) (0 + y) 78 78 1.7
  where 
    direction = view lensDirection pacman
    x = if not (isLeavingCell pacman)
            then 0
            else 78
    y = case direction of
            LEFT  -> 0 * 78
            RIGHT -> 1 * 78
            UP    -> 2 * 78
            DOWN  -> 3 * 78

-- pacSpriteNormal :: SpriteSheet t -> Pacman -> TextureBox t
-- pacSpriteNormal sheet pacman = 
--     mkTextureBox sheet $ SpriteConfig (81 + x) (1 + y) 17 17 1.7
--   where 
--     direction = view lensDirection pacman
--     x = if not (isLeavingCell pacman)
--             then 0
--             else 20
--     y = case direction of
--             LEFT  -> 0
--             RIGHT -> 20
--             UP    -> 40
--             DOWN  -> 60

pacSpriteFull :: SpriteSheet t -> Pacman -> TextureBox t
pacSpriteFull sheet pacman = 
    mkTextureBox sheet $ SpriteConfig (312 + 2 * 78) (0 + y) 78 78 1.7
  where 
    y = case view lensDirection pacman of
            LEFT  -> 0 * 78
            RIGHT -> 1 * 78
            UP    -> 2 * 78
            DOWN  -> 3 * 78

pacSpriteDying :: SpriteSheet t -> Pacman -> Int -> TextureBox t
pacSpriteDying sheet _ i = mkTextureBox sheet $ SpriteConfig (0 + 78 * fromIntegral i) 938 78 78 1.7

-- pacSpriteDying :: SpriteSheet t -> Pacman -> Int -> TextureBox t
-- pacSpriteDying sheet _ i = mkTextureBox sheet $ SpriteConfig (1 + 20 * fromIntegral i) 239 17 17 1.7


ghostSprite :: SpriteSheet t -> Ghost -> GhostPhase -> TextureBox t
ghostSprite sheet ghost phase = 
    mkTextureBox sheet $ SpriteConfig (0 + x) (312 + y) 78 78 1.7
  where 
    name = ghostId ghost
    mode = ghostMode ghost
    direction = ghostDirection ghost 
    retreating = ghostRetreating ghost > 0
    blink = isLeavingCell ghost
    colorOffset
        | retreating       = 6 
        | mode == FRIGHTEN = 4
        | otherwise = 
            case name of 
                INKY   -> 2
                BLINKY -> 0
                PINKY  -> 1
                CLYDE  -> 3
    dirOffset = 
        if mode == FRIGHTEN
            then if phaseDuration phase < 3 && fmod (phaseDuration phase) 0.8 < 0.4
                    then 1 
                    else 0
        else case direction of 
                LEFT  -> 2
                RIGHT -> 3
                UP    -> 0
                DOWN  -> 1
    blinkOffset = if blink then 0 else 1
    x = 78 * if retreating 
                then dirOffset
                else 2 * dirOffset + blinkOffset
    y = 78 * colorOffset

--ghostSprite :: SpriteSheet t -> Ghost -> GhostPhase -> TextureBox t
--ghostSprite sheet ghost phase = 
--    mkTextureBox sheet $ SpriteConfig (1 + x) (1 + y) 17 17 1.7
--  where 
--    name = ghostId ghost
--    mode = ghostMode ghost
--    direction = ghostDirection ghost 
--    retreating = ghostRetreating ghost > 0
--    blink = isLeavingCell ghost
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
--



class Monad m => Font m f where

    textWidth :: f -> String -> Float -> m Float
    -- textWidth font text size = 

    drawText :: f -> String -> Float -> Float -> Float -> Color -> m ()
    -- drawText font text x y size


drawHeader :: (Texture m t, Font m f) 
     => SpriteSheet t -> f -> LabelConfig -> GameState -> LevelPhase -> String -> Bool -> m ()
drawHeader sheet font labels state phase lvlname muted = do

    let message = case phase of
            GAME_OVER -> Just (Color 1 0 0 1, lbGameOver labels)
            STARTING  -> Just (Color 1 1 0 1, lbStarting labels)
            RESPAWN   -> Just (Color 1 1 0 1, lbRespawn labels)
            _         -> Nothing
        score = gsScore  state
        n     = gsLifes  state
        headerWidth  = 45.0 

    drawText font (lbLevel labels ++ map toUpper lvlname) 0 3.25 1 (Color 1 1 1 1)
    drawText font (lbScore labels ++ show score) 0 0.75 1 (Color 1 1 1 1)
    
    drawMute sheet (headerWidth - 2, 3) 2 muted
    drawLifes sheet (headerWidth, 0.5) 1.5 n
    case message of
        Nothing -> return ()
        Just (col, txt) -> do
            width <- textWidth font txt 1
            drawText font txt (headerWidth / 2 - width / 2) 0.75 1 col

drawPreviewHeader :: (Texture m t, Font m f) 
     => SpriteSheet t -> f -> LabelConfig -> String -> Bool -> m ()
drawPreviewHeader sheet font labels lvlname muted = do
    let headerWidth  = 45.0 
    drawText font (lbLevel labels ++ map toUpper lvlname) 0 3.25 1 (Color 1 1 1 1)
    drawText font (lbScore labels) 0 0.75 1 (Color 1 1 1 1)
    drawMute sheet (headerWidth - 2, 3) 2 muted
    drawLifes sheet (headerWidth, 0.5) 1.5 3 -- TODO don't hardcode
    

drawLifes :: (Texture m t) => SpriteSheet t -> (Float, Float) -> Float -> Int -> m ()
drawLifes sheet (x, y) size n = do
    forM_ [1..n] $ \i -> do
        drawSprite' sprite (x - 1.5 * size * fromIntegral i, y) (size, size)
  where 
    sprite = mkTextureBox sheet $ sprtLife $ sheetConfig sheet
           

drawMute :: Texture m t => SpriteSheet t -> (Float, Float) -> Float -> Bool -> m ()
drawMute sheet (x, y) size muted = do 
    drawSprite' sprite (x, y) (size, size)                
  where 
    sprite = if muted 
                then mkTextureBox sheet $ sprtMuted $ sheetConfig sheet
                else mkTextureBox sheet $ sprtUnmted $ sheetConfig sheet
                
-- drawCircleSegment :: (Float, Float) -> Float -> Int -> (Int, Int) -> IO ()
-- drawCircleSegment (x, y) radius n (start, stop) =
--     renderPrimitive TriangleFan $ do
--         vertex $ Vertex2 x y
--         forM_ [start .. stop] $ \i -> do 
--             let theta = 2 * pi * fromIntegral i / fromIntegral n
--             vertex $ Vertex2 (x + radius * cos theta) (y + radius * sin theta)



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

-- data Transformation = Transformation
--   { transScaleX :: Float
--   , transScaleY :: Float
--   , transMoveX  :: Float
--   , transMoveY  :: Float
--   } deriving (Eq, Show)
-- 
-- Canvas: GLUT Window / HTML Canvas

-- Canvas -> Context: OpenGL / 2D Canvas / WebGL

-- clear (background color)

-- translate 
-- scale
-- push / pop / reset transformation (set / get)

-- colored shapes
-- textures
-- fonts
{--
class Monad m => Canvas m c | c -> m where 
    width        :: m Int
    height       :: m Int 
    clear        :: Color -> m ()
    setViewport  :: Rect -> Rect -> m ()
    translate    :: Float -> Float -> m ()
    scale        :: Float -> Float -> m ()
    getTransform :: m ()
    setTransfrom :: () -> m ()


class DrawContext m c => Drawable c a where
    draw :: c -> a -> m ()


 
--}