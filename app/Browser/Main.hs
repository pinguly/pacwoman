{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (when, forM, forM_)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (State, execState)
import Control.Monad.State.Class (get, put, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Cont (ContT(..), evalContT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.State.Strict (StateT, execStateT, runStateT, evalStateT)

import qualified Data.Array as Array
import qualified Data.Map as Map
import Data.Coerce (coerce)
import Data.IORef

import GHC.JS.Prim (JSVal)
import qualified GHC.JS.Prim
import GHC.JS.Foreign.Callback (Callback)

import Game.Map.Config
import Animation
import Game.State 
import Game.Monad
import Game.Map.Loading 
import Game.Map.Chart 
import Game.Enums
import Game.Pacman
import Game.Ghost
import Config


import Data.Char (toUpper)


import JS
import WebGL
import Graphics

import Game.Monad

import Audio (WebAudioContext, WebSound, initWebAudio, loadWebSound)
import BitmapFont (BitmapFont(..))

import Sounding as Audio
import Data.Maybe (catMaybes, Maybe (Nothing))
import GHC.Num (Num(fromInteger))
import GHC.Real (fromIntegral)
import Drawing --(Rect(..), SpriteSheet(..), MonadDraw (..), MonadCanvas(..))
import Control.Monad.State (runState)
import UI
import Game.Monad (LevelRefs)
import Control.Lens (levels, preview, Level)
import Game.State (LevelPhase (GAME_OVER))
import Sounding (MonadAudio(isMute))
import Control.Monad.Reader (ReaderT(runReaderT))


foreign import javascript "setInterval"
  js_setInterval :: Callback (IO ()) -> Int -> IO ()

foreign import javascript "setTimeout"
  js_setTimeout :: Callback (IO ()) -> Int -> IO ()

foreign import javascript "window.requestAnimationFrame"
  js_window_requestAnimationFrame :: Callback (IO ()) -> IO ()

foreign import javascript "(() => window.innerWidth)"
  js_window_innerWidth :: IO Int

foreign import javascript "console.log"
  js_console_log :: JSVal -> IO ()

foreign import javascript "(() => gl)"
    js_gl :: IO WebGLRenderingContext

foreign import javascript "(() => window.performance.now())"
    js_date_now :: IO Float

foreign import javascript "(() => events.shift())"
    js_pollEvent :: IO JSVal

foreign import javascript "(() => Math.random())"
    js_random :: IO Float





data GlobalState = GlobalState 
  { globGLContext   :: !GLContext
  , globConfig      :: !GlobalConfig
  , globLevels      :: ![LevelConfig]
  , globTime        :: !Float
  , globDeltaTime   :: !Float
  , globDirInput    :: Maybe Direction
  , globSheet       :: SpriteSheet Graphics.Texture
  , globAudio       :: WebAudioContext
  , globSounds      :: Map.Map Game.Enums.Sound WebSound
  , globFont        :: BitmapFont Graphics.Texture
  , globStaticMaze  :: Float32Array 
  , globStaticItems :: Float32Array
  , globStaticBlink :: Float32Array
  , globStaticICells :: Map.Map Cell (Int, Int)
  , globStaticBCells :: Map.Map Cell (Int, Int)
  }

instance MonadRandom (StateT LevelRefs (ContT LevelRefs UI)) where 
    random = liftIO js_random


instance MonadGame (StateT LevelRefs (ContT LevelRefs UI)) where


    display lvlPhase duration = do
        lvlRefs <- get
        lift . lift $ uiDrawLevel lvlRefs lvlPhase
        t <- liftIO js_date_now
        t' <- globTime <$> lift (lift get)
        -- liftIO $ consoleLog $ "run time: " ++ show (t - t')

        let loop = do
                liftIO _pollEvent >>= \case 
                    Just (Arrow direction) -> 
                        lift . lift $ modify $ \state -> state { globDirInput = Just direction }
                    Just Mute -> do
                        audio <- globAudio <$> lift (lift get)
                        liftIO $ runReaderT toggleMute audio
                        lift . lift $ uiDrawLevel lvlRefs lvlPhase
                    Just Resize -> 
                        lift . lift $ uiDrawLevel lvlRefs lvlPhase
                    Just Back -> do 
                        clearAllSound
                        lift . ContT . const $ return lvlRefs
                    _ -> return ()
                t' <- liftIO js_date_now
                when (t' <= t + fromIntegral duration) $ do
                    lift . lift . lift $ JS.sleep 32
                    loop

        t' <- liftIO js_date_now
        loop
        t <- liftIO js_date_now
        -- liftIO $ consoleLog $ "event time: " ++ show (t - t') ++ " (" ++ show duration ++ ")"
        
        t <- liftIO js_date_now
        lift . lift $ modify $ \state -> state {globDeltaTime = (t - globTime state - fromIntegral duration) / 1000}
        lift . lift $ modify $ \state -> state {globTime = t}
        

    inputDirection = do
        direction <- globDirInput <$> lift (lift get)
        lift . lift $ modify $ \state -> state { globDirInput = Nothing }
        return direction

    getDeltaTime = do
        dt <- globDeltaTime <$> lift (lift get)
        t <- liftIO js_date_now
        -- liftIO $ consoleLog $ "delta time: " ++ show (t, dt)
        return dt
        

    playSound sound = do
        ctx <- globAudio <$> lift (lift get)
        sounds    <- globSounds <$> lift (lift get)
        liftIO $ flip runReaderT ctx $ do
            forM_ (Map.lookup sound sounds) Audio.playSound

    stopSound sound = do
        ctx <- globAudio <$> lift (lift get)
        sounds <- globSounds <$> lift (lift get)
        liftIO $ flip runReaderT ctx $ do
            forM_ (Map.lookup sound sounds) Audio.stopSound



loadPacSounds :: WebAudioContext -> Map.Map Game.Enums.Sound String -> JSIO (Map.Map Game.Enums.Sound WebSound)
loadPacSounds ctx paths = do
    Map.fromList <$> (forM (Map.assocs paths) $ \(key, value) -> do
        let looping = case key of
                        SIREN        -> True
                        FRIGHTENED   -> True
                        _            -> False
        sound <- loadWebSound ctx looping value
        return (key, sound))



data LevelPreview = LevelPreview
  { previewBody   :: Float32Array
  , previewRefs   :: LevelRefs
  , previewConfig :: LevelConfig
  }

_pollEvent :: IO (Maybe Event)
_pollEvent = do
    event <- js_pollEvent
    if GHC.JS.Prim.isUndefined event 
        then 
            return Nothing
        else do
            type_ <- GHC.JS.Prim.fromJSString <$> GHC.JS.Prim.getProp event "type"
            case type_ of 
                "keydown" -> do
                    key <- GHC.JS.Prim.fromJSString <$> GHC.JS.Prim.getProp event "key"
                    return $ case key of 
                        "ArrowLeft"  -> Just (Arrow LEFT)
                        "ArrowRight" -> Just (Arrow RIGHT)
                        "ArrowUp"    -> Just (Arrow UP)
                        "ArrowDown"  -> Just (Arrow DOWN)
                        "a"          -> Just (Arrow LEFT)
                        "d"          -> Just (Arrow RIGHT)
                        "w"          -> Just (Arrow UP)
                        "s"          -> Just (Arrow DOWN)
                        "Backspace"  -> Just Back
                        "Escape"     -> Just Back
                        "Enter"      -> Just Enter
                        " "          -> Just Enter
                        -- "m"          -> Just Mute
                        "f"          -> Just Fullscreen
                        _            -> Nothing
                "resize"  -> return $ Just Resize
                _         -> return Nothing 

instance MonadUI (StateT GlobalState JSIO) where

    type DrawContext (StateT GlobalState JSIO) = DrawStack ()
    type Preview (StateT GlobalState JSIO) = LevelPreview

    -- pollEvent :: IO (Maybe Event)
    pollEvent = liftIO _pollEvent

    drawScene scene = do
        context <- globGLContext <$> get
        liftIO . requestAnimationFrame $ do
            t <- liftIO js_date_now
            array0 <- toFloat32Array []
            runReaderT (execStateT scene array0) context
            return ()
            -- t' <- liftIO js_date_now
            -- liftIO $ consoleLog $ "render time: " ++ show (t' - t)
            

    -- mkPreview :: LevelConfig -> UI LevelPreview
    mkPreview config = do
        sheet <- globSheet <$> get -- TODO make implicit
        body <- liftIO $ drawStatic $ drawLevel sheet lvlRefs GAME_OVER
        return $ LevelPreview body lvlRefs config
      where
        state = makeGameState config
        lvlRefs = LevelRefs state state

    -- uiDrawLevel :: LevelRefs -> LevelPhase -> UI ()
    uiDrawLevel lvlRefs lvlphase = do
        sheet <- globSheet <$> get
        audio <- globAudio <$> get
        font  <- globFont  <$> get
        staticMaze  <- globStaticMaze <$> get
        staticItems <- globStaticItems <$> get
        staticBlink <- globStaticBlink <$> get
        staticICells <- globStaticICells <$> get
        staticBCells <- globStaticBCells <$> get
        context  <- globGLContext <$> get
        labels <- cfgLabels . globConfig <$> get
        lvlname <- cfgName . head . globLevels <$> get
        muted <- liftIO $ runReaderT isMute audio
        let state = lvlState lvlRefs
        case Map.lookup (pacCell $ gsPacman state) staticICells of 
            Just (start, len) -> liftIO $ js_fill staticItems 0 start (start + len)
            Nothing -> return ()
        case Map.lookup (pacCell $ gsPacman state) staticBCells of 
            Just (start, len) -> liftIO $ js_fill staticBlink 0 start (start + len)
            Nothing -> return ()
        drawScene $ do
            clearCanvas (Color 0.6 0.6 0.6 1)
            levelViewport lvlRefs

            let wallcolor = case lvlphase of
                    WINNING i ->
                        if even i then Color 0 0 1 1 else Color 0.5 0.5 1 1
                    _         -> Color 0 0 1 1
            liftIO $ do 
                setColor context wallcolor
                drawJsArray context gl_TRIANGLES staticMaze
                setColor context (Color 1 1 1 1)

            let t = gsTime state
                interval = cfgDotBlink . cfgTiming $ gsConfig state
                blink = fmod t (2 * interval) <= interval
            liftIO $ drawJsArray context gl_TRIANGLES staticItems

            when blink $
                liftIO $ drawJsArray context gl_TRIANGLES staticBlink

            -- concat . reverse . flip execState []

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

            headerViewport
            
            let message = case lvlphase of
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
                    liftIO $ setColor context col
                    width <- textWidth font txt 1
                    drawText font txt (headerWidth / 2 - width / 2) 0.75 1 col
                    liftIO $ setColor context (Color 1 1 1 1)

    uiDrawPreview preview = do
        sheet <- globSheet <$> get -- TODO make implicit
        audio <- globAudio <$> get
        font  <- globFont  <$> get
        labels <- cfgLabels . globConfig <$> get
        muted <- liftIO $ runReaderT isMute audio
        context <- globGLContext <$> get
        let lvlname = cfgName . previewConfig $ preview
        --liftIO $ consoleLog "request draw"
        drawScene $ do 
            --liftIO $ consoleLog "start draw"
            clearCanvas (Color 0.6 0.6 0.6 1)
            levelViewport (previewRefs preview) 
            -- t <- liftIO js_date_now
            liftIO $ drawJsArray context gl_TRIANGLES (previewBody $ preview)
            -- t' <- liftIO js_date_now
            -- liftIO $ consoleLog $ "drawJsArray: " ++ show (t' - t)
            headerViewport
            drawPreviewHeader sheet font labels lvlname muted

    uiToggleAudio = do
        audio <- globAudio <$> get
        liftIO $ runReaderT toggleMute audio

    sleep = lift . JS.sleep

    evalLevel previews = do
        t <- liftIO js_date_now
        sheet <- globSheet <$> get -- TODO make implicit
        arrayMaze <- liftIO $ drawStatic $ drawCharts (gsCharts state) (Color 1 1 1 1)

        let items  = filter ((/= REVERSE) . snd) . Map.assocs $ gsItems state
            blinks = filter ((== REVERSE) . snd) . Map.assocs $ gsItems state
        itemData <- forM items $ \(cell, item) -> do
                        let buffer = concat $ reverse $ flip execState [] $ drawItem sheet cell item True
                        return (cell, buffer)
        blinkData <- forM blinks $ \(cell, item) -> do
                        let buffer = concat $ reverse $ flip execState [] $ drawItem sheet cell item True
                        return (cell, buffer)
        arrayItems <- liftIO $ toFloat32Array $ concat $ map snd itemData
        arrayBlink <- liftIO $ toFloat32Array $ concat $ map snd blinkData
        let itemLens  = map (length . snd) itemData
            blinkLens = map (length . snd) blinkData
            itemMap = Map.fromList $ zip (map fst itemData) $ zip (scanl (+) 0 itemLens) itemLens
            blinkMap = Map.fromList $ zip (map fst blinkData) $ zip (scanl (+) 0 blinkLens) blinkLens
        modify $ \state -> state 
          { globTime = t
          , globDeltaTime = 0
          , globDirInput = Nothing
          , globLevels = map previewConfig previews
          , globStaticMaze = arrayMaze
          , globStaticItems = arrayItems
          , globStaticBlink = arrayBlink
          , globStaticICells = itemMap
          , globStaticBCells = blinkMap
          }
        evalContT $ execStateT loopStarting lvlRefs
      where 
        lvlRefs = previewRefs (head previews)
        state = lvlState lvlRefs
        

initState :: JSIO GlobalState
initState = do
    --liftIO $ consoleLog "Fetch resources"
    config <- fetchJSON "assets/config.json"
    img <- fetchImage . sprtSheet $ cfgSprites config
    bitmap <- fetchImage "assets/fonts/charmap-cellphone_white.png"
    levels <- forM (cfgLevels config) fetchJSON
                  
    --liftIO $ consoleLog "Init audio"
    audio <- liftIO initWebAudio
    sounds <- loadPacSounds audio (cfgSounds config)

    --liftIO $ consoleLog "Init gl"
    gl <- liftIO js_gl
    context <- liftIO $ initGL gl

    texture <- liftIO $ loadTexture gl 0 img
    let width  = texWidth texture
        height = texHeight texture
        sheet  = SpriteSheet texture (fromIntegral width) (fromIntegral height) (cfgSprites config)

    texture <- liftIO $ loadTexture gl 1 bitmap
    let width  = texWidth texture
        height = texHeight texture
        font = BitmapFont texture width height 0 1 7 9 7 7 18 ' '

    -- liftIO $ consoleLog "Finish Init"
    t <- liftIO js_date_now
    array <- liftIO $ toFloat32Array []
    return $ 
        GlobalState context config levels t 0 Nothing sheet audio sounds font array array array Map.empty Map.empty


type UI = StateT GlobalState JSIO

foreign import javascript "(($1, $2, $3, $4) => $1.fill($2, $3, $4))"
    js_fill :: Float32Array -> Float -> Int -> Int -> IO ()


        



main :: IO ()
main = runJSIO $ do
    --liftIO $ consoleLog "start main"
    --_ <- createWindow "Pac-Woman"
    state <- initState
    flip runStateT state $ do
        levels <- globLevels <$> get
        previews <- forM levels mkPreview
        sceneLevelPreview previews
