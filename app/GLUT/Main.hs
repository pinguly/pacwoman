{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

import Data.IORef
    ( IORef, modifyIORef, newIORef, readIORef, writeIORef, atomicModifyIORef )
import Graphics.UI.GLUT hiding ( get )
import qualified Graphics.UI.GLUT
import qualified Data.Array as Array

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map

import Config

import Game.Enums
import Game.Map.Chart
import Game.Map.Loading
import Game.Pacman
import Game.State
import Game.Monad
import qualified UI.Font as Font

-- import UI.SpriteSheet ( loadSheetFromPng, SpriteSheet )

import Data.Time.Clock ( UTCTime, diffUTCTime, getCurrentTime )

import Animation
import Draw

import Sound.ALUT ( runALUT )
import UI.Sound

import Control.Monad.Trans.Cont ( ContT(..), evalContT )
import Control.Monad.Cont.Class ( callCC )
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.IO.Class
import Game.Map.Config (LevelConfig(..), TimingConfig(..))

-- import Game.Map.Dijkstra (main)
import Data.Char (toUpper)
--import Control.Monad.Trans.State

import Control.Monad
import Control.Monad.Trans.State.Lazy (StateT (runStateT), evalStateT)
import Control.Monad.State.Class (get)
import Control.Monad.Trans.Class (lift)
import Graphics.GL (glLoadIdentity)
import Control.Lens (Lens', lens, modifying, use)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State (execStateT)
import UI.Sound (loadALSound, ALContext, defaultALContext)
import Config (GlobalConfig(cfgSounds))
import Data.Tuple (Solo)

import Control.Monad.Random (getRandom)
import Game.Ghost (MonadRandom(..))

import Drawing hiding (headerViewport)

import Sounding hiding (startSound, stopSound, Sound)
import qualified Sounding as Audio

-- import Paths_pacwoman
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return

{-

Loading:
- loadTexture, loadJSON, loadSound, loadFont

MonadTimerCallback -> sleep

bindKey :: Char -> IO () -> IO (), unbindKey :: Char -> IO ()

Basic game loop: exit, loop, subloop

-}

type Timer = IORef UTCTime

newTimer :: MonadIO m => m Timer
newTimer = liftIO $ getCurrentTime >>= newIORef

-- Datentyp, Globale Variablen. Karten, Spieler, Items, Punktzahl, Geist und Gameover
-- Referenzen.
data GlobalRefs = GlobalRefs
  { globFont        :: Font.Font
  , globSprites     :: SpriteSheet IO
  , globALContext   :: ALContext
  , globSounds      :: Map Sound ALSound
  , globLevels      :: IORef [LevelConfig]
  , globLabels      :: LabelConfig
  , globKeyBindings :: Map Char (IO ())
  -- underlying intermediate data
  , globInputDir    :: IORef (Maybe Direction)
  , globInputExit   :: IORef Bool
  , globTimer       :: Timer
  }

lensKeyBindings :: Lens' GlobalRefs (Map Char (IO ()))
lensKeyBindings = lens globKeyBindings $ \refs bindings -> refs { globKeyBindings = bindings }

class Monad m => MonadTimerCallback m where 
    timerCallback :: Int -> m () -> m ()

instance MonadTimerCallback IO where
    timerCallback = addTimerCallback
    -- setCallback :: Int -> m () -> m c
    -- cancelCallback :: c -> m ()

class Monad m => MonadSleep m where 
    sleep :: Int -> m ()


type GameLoopT m = MaybeT (ContT () m)

runGameLoop :: Monad m => GameLoopT m a -> m ()
runGameLoop = evalContT . void . runMaybeT

instance MonadTimerCallback m => MonadSleep (GameLoopT m) where
    sleep duration =
        callCC $ \next -> do
            lift . lift . timerCallback duration . runGameLoop $ next ()
            mzero



-- sleep :: Int -> GameLoop ()
-- sleep duration =
--     callCC $ \next -> do
--         liftIO $ addTimerCallback duration $ runGameLoop $ next ()
--         mzero

type LevelLoop = MaybeT (StateT LevelRefs (StateT GlobalRefs (GameLoopT IO)))

runLevelLoop :: LevelLoop a -> LevelRefs -> StateT GlobalRefs (GameLoopT IO) ()
runLevelLoop level lvlRefs = do
    globRefs <- get
    liftIO $ writeIORef (globInputDir globRefs) Nothing
    liftIO $ writeIORef (globInputExit globRefs) False
    liftIO getCurrentTime >>= liftIO . writeIORef (globTimer globRefs)
    evalStateT (void $ runMaybeT level) lvlRefs



bindKey :: (MonadState GlobalRefs m, MonadIO m) => Char -> IO () -> m ()
bindKey charCode event = do
    liftIO $ print charCode
    modifying lensKeyBindings $
        Map.insert charCode event
    bindings <- use lensKeyBindings
    keyboardCallback $= Just (\c _ -> Map.findWithDefault (return ()) c bindings)

unbindKey :: (MonadState GlobalRefs m, MonadIO m) => Char -> m ()
unbindKey keyCode = do
    modifying lensKeyBindings $ Map.delete keyCode
    bindings <- use lensKeyBindings
    keyboardCallback $= Just (\c _ -> Map.findWithDefault (return ()) c bindings)


-- MonadFail
loadPacSounds :: MonadIO m => ALContext -> Map Sound FilePath -> m (Map Sound ALSound)
loadPacSounds ctx paths = do
    Map.fromList <$> (liftIO $ forM (Map.assocs paths) $ \(key, value) -> do
        let looping = case key of
                        SIREN        -> True
                        FRIGHTENED   -> True
                        _            -> False
        sound <- loadALSound ctx looping value
        return (key, sound))
    
    -- $ \ path -> do
    --     let a = path 
    --     loadALSound ctx 1 path

    -- sounds <- mapM loadSound paths
    -- loopingMode (audioSourceLoop audio) $= Looping
    -- loopingMode (audioSourceOnce audio) $= OneShot
    -- loopingMode (audioSourceEaten audio) $= Looping
    -- return $ ALUTContext sounds False

-- soundSource :: Sound -> Audio -> Source
-- soundLooping = \case
--     SIREN        -> Looping
--     FRIGHTENED   -> Looping
--     -            -> OneShot

-- MonadDraw :: draw
-- MonadIO :: getCurrentTime
-- MonadAudio?

-- draw:
--- isMuted, Levelname => Header
--- GameState

instance MonadRandom LevelLoop where 

    random = liftIO getRandom

instance MonadGame LevelLoop where

    display lvlPhase duration = do
        lvlRefs   <- lift get
        globRefs  <- lift $ lift get
        displayCallback $= draw globRefs lvlRefs lvlPhase
        liftIO $ postRedisplay Nothing
        startTime <- liftIO getCurrentTime
        liftIO getCurrentTime >>= liftIO . writeIORef (globTimer globRefs)
        callCC $ \break -> forever $ do
            lift $ lift $ lift $ sleep 16
            liftIO (readIORef (globInputExit globRefs)) >>= \case
                True  -> do
                    forM_ [minBound .. maxBound] stopSound
                    lvlRefs   <- lift get
                    displayCallback $= draw globRefs lvlRefs GAME_OVER
                    liftIO $ postRedisplay Nothing
                    mzero
                False -> return ()
            currTime <- liftIO getCurrentTime
            when (10^3 * realToFrac (diffUTCTime currTime startTime) > fromIntegral duration) $ -- TODO Double
                break ()
            liftIO $ writeIORef (globTimer globRefs) currTime


    inputDirection = do
        inputRef <- globInputDir <$> lift (lift get)
        liftIO $ atomicModifyIORef inputRef $ (,) Nothing

    getDeltaTime = do
        oldTime  <- lift (lift get) >>= liftIO . readIORef . globTimer
        currTime <- liftIO getCurrentTime
        return $ realToFrac $ diffUTCTime currTime oldTime

    playSound sound = do
        alContext <- globALContext <$> lift (lift get)
        sounds    <- globSounds   <$> lift (lift get)
        liftIO $ flip runReaderT alContext $ do
            forM_ (Map.lookup sound sounds) Audio.playSound

    stopSound sound = do
        alContext <- globALContext <$> lift (lift get)
        sounds <- globSounds <$> lift (lift get)
        liftIO $ flip runReaderT alContext $ do
            forM_ (Map.lookup sound sounds) Audio.stopSound

setViewport :: (Double, Double) -> (Double, Double) -> Position -> Size -> Maybe Direction -> IO ()
setViewport (coordX, coordY) (coordWidth, coordHeight) 
            (Position screenX screenY) (Size screenWidth screenHeight)
            alignment 
    = do  
        matrixMode $= Projection
        loadIdentity
        ortho coordX (coordX + coordWidth) coordY (coordY + coordHeight) (-1) 1
        viewport $=
            if aspect >= aspect'
                then (Position (screenX + alignX * offsetX) screenY,
                      Size (screenWidth - 2 * offsetX) screenHeight)
                else (Position screenX (screenY + alignY * offsetY), 
                      Size screenWidth (screenHeight - 2 * offsetY))
  where 
    aspect  = realToFrac screenWidth / realToFrac screenHeight :: Double
    aspect' = coordWidth  / coordHeight  :: Double
    offsetX = round $ (aspect - aspect') / 2 * realToFrac screenHeight
    offsetY = round $ (1 / aspect - 1 / aspect') / 2 * realToFrac screenWidth
    alignX = case alignment of 
                Just LEFT  -> 0
                Just RIGHT -> 2
                _          -> 1
    alignY = case alignment of 
                Just DOWN  -> 0
                Just UP    -> 2
                _          -> 1
    
    
        

reshape :: LevelRefs -> ReshapeCallback
reshape refs (Size screenWidth screenHeight) = do
    let charts = Array.elems . gsCharts .  lvlState $ refs
        -- ix = realToFrac . cfgInfoX . cfgInfo . lvlConfig $ refs
        -- iy = realToFrac . cfgInfoY . cfgInfo . lvlConfig $ refs
    let borderL = round $ 0.06 * realToFrac screenWidth
        borderR = round $ 0.06 * realToFrac screenWidth
        borderU = round $ 0.15 * realToFrac screenHeight
        borderD = round $ 0.06 * realToFrac screenHeight
        reducedWidth  = screenWidth  - borderL - borderR
        reducedHeight = screenHeight - borderU - borderD
        minX = realToFrac . minimum . map (fst . chartOrigin) $ charts
        maxX = realToFrac . maximum . map (fst . chartOrigin) $ charts
        minY = realToFrac . minimum . map (snd . chartOrigin) $ charts
        maxY = realToFrac . maximum . map (snd . chartOrigin) $ charts
        width'  = (maxX + 1) - minX
        height' = (maxY + 1) - minY
    setViewport 
        (minX, minY) (width', height')
        (Position borderL borderD) (Size reducedWidth reducedHeight)
        (Just UP)
    

headerViewport :: IO ()
headerViewport = do
    Size screenWidth screenHeight <- Graphics.UI.GLUT.get windowSize
    let borderL = round $ 0.06 * realToFrac screenWidth
        borderR = round $ 0.06 * realToFrac screenWidth
        borderU = round $ 0.12 * realToFrac screenHeight
        reducedWidth  = screenWidth - borderL - borderR
        reducedHeight = round $ realToFrac screenHeight * 0.1
    setViewport 
        (0, 0) (45, 5)
        (Position borderL (screenHeight - borderU) ) (Size reducedWidth reducedHeight)
        (Just DOWN)

-- Zeichnet das Spielfeld, nimmt dafür die Information über die Karten, Spieler,
-- Geister und Items.
draw :: GlobalRefs -> LevelRefs -> LevelPhase -> DisplayCallback
draw globRefs lvlRefs lvlphase = do

    Graphics.UI.GLUT.get windowSize >>= liftIO . reshape lvlRefs

    clear [ ColorBuffer ]
    
    matrixMode $= Modelview 0
    glLoadIdentity

    -- drawSquare (-100, -100) 200 (Color3 1 0 0)
    let state = lvlState lvlRefs
    let font  = globFont globRefs
        sheet = globSprites globRefs
        labels = globLabels globRefs

    drawLevel sheet lvlRefs lvlphase

    let message = case lvlphase of
            GAME_OVER -> Just (Color3 1 0 0, lbGameOver labels)
            STARTING  -> Just (Color3 1 1 0, lbStarting labels)
            RESPAWN   -> Just (Color3 1 1 0, lbRespawn labels)
            _         -> Nothing


    let score   = gsScore  state
        n       = gsLifes  state
    muted <- runReaderT isMute (globALContext globRefs) 
    lvlname <- cfgName . head <$> readIORef (globLevels globRefs)

    headerViewport

    matrixMode $= Modelview 0
    glLoadIdentity

    let headerWidth  = 45.0 
    preservingMatrix $ do
        translate $ Vector3 (0 :: Float) (3.25) 0
        color (Color3 (1::Float) 1 1)
        Font.renderText font $ lbLevel labels ++ map toUpper lvlname
    preservingMatrix $ do
        translate $ Vector3 (0 :: Float) (0.75) 0
        color (Color3 (1::Float) 1 1)
        Font.renderText font $ lbScore labels ++ show score
    preservingMatrix $ do
        translate $ Vector3 (headerWidth - 2 :: Float) (3.25 - 0.25) 0
        color (Color3 (1::Float) 1 1)
        drawMute sheet (0, 0) 2 muted
    preservingMatrix $ do
        translate $ Vector3 (headerWidth :: Float) (0.75 - 0.25) 0
        color (Color3 (1::Float) 1 1)
        drawLifes sheet (0, 0) 1.5 n
    preservingMatrix $ do
        case message of
            Nothing -> return ()
            Just (col, txt) -> do
                translate $ Vector3 (headerWidth / 2 - Font.textWidth font txt / 2 :: Float) (0.75) 0
                color (col :: Color3 Float)
                Font.renderText font txt

    swapBuffers


specialKeypressGame :: GlobalRefs -> SpecialCallback
specialKeypressGame globRefs KeyUp    _ = globInputDir globRefs $= Just UP
specialKeypressGame globRefs KeyDown  _ = globInputDir globRefs $= Just DOWN
specialKeypressGame globRefs KeyLeft  _ = globInputDir globRefs $= Just LEFT
specialKeypressGame globRefs KeyRight _ = globInputDir globRefs $= Just RIGHT
specialKeypressGame _        _        _ = return ()


-- Ausführungen von Dingen am Anfang des Spieles.
loadLevel :: StateT GlobalRefs (GameLoopT IO) ()
loadLevel = do
    -- Laden der Karten
    globRefs <- get

    bindKey '\DC1' leaveMainLoop
    bindKey '\ACK' fullScreenToggle
    bindKey 'm' $ do
        let ctx = globALContext globRefs
        flip runReaderT ctx $
            toggleMute
        postRedisplay Nothing


    config <- liftIO $ head <$> readIORef (globLevels globRefs)


    let state = makeGameState config
        animations = Animations
          { animDotBlink   = newBlink (cfgDotBlink   . cfgTiming $ config)
          }

    --Erstellung der Globalenvariablen
    let lvlRefs = LevelRefs state state
                    

    -- displayCallback   $= draw globRefs lvlRefs GAME_OVER
    --reshapeCallback   $= Just (reshape lvlRefs)
    reshapeCallback   $= Just (\_ -> postRedisplay Nothing)
    
    forM_ ['\r', ' '] unbindKey
    bindKey 'w'    $ globInputDir globRefs $= Just UP
    bindKey 's'    $ globInputDir globRefs $= Just DOWN
    bindKey 'a'    $ globInputDir globRefs $= Just LEFT
    bindKey 'd'    $ globInputDir globRefs $= Just RIGHT
    bindKey '\ESC' $ globInputExit globRefs $= True

    specialCallback   $= Just (specialKeypressGame globRefs)

    runLevelLoop loopStarting lvlRefs
    
    forM_ ['w', 'a', 's', 'd', '\ESC'] unbindKey
    bindKey '\r' $ runGameLoop (evalStateT loadLevel globRefs) -- TODO press at same time?
    bindKey ' '  $ runGameLoop (evalStateT switchLevel globRefs)

    specialCallback   $= Nothing

switchLevel :: StateT GlobalRefs (GameLoopT IO) ()
switchLevel = do
    globRefs <- get
    liftIO $ do
        modifyIORef (globLevels globRefs) $ (++) . tail <*> return . head
        
        config <- liftIO $ head <$> readIORef (globLevels globRefs)
        let state = makeGameState config
            animations = Animations
                { animDotBlink   = newBlink (cfgDotBlink   . cfgTiming $ config)
                }
        --Erstellung der Globalenvariablen
        let lvlRefs = LevelRefs state state
        displayCallback   $= draw globRefs lvlRefs GAME_OVER
        postRedisplay Nothing


main :: IO ()
main  = runALUT "" [] $ \_ _ -> do
    _ <- getArgsAndInitialize
    initialDisplayMode    $= [RGBMode, DoubleBuffered]
    initialWindowPosition $= Position 0 0
    initialWindowSize     $= Size 1000 800
    _ <- createWindow "Pac-Woman"
    clearColor $= Color4 0.5 0.5 0.5 0.5

    config <- eitherDecodeFileStrict "assets/config.json" >>= \case
                Left err     -> fail err
                Right config -> return config

    levels <- mapM loadConfig (cfgLevels config)
    when (null levels) $ -- TODO move to parser
        fail "no levels found!"

    alContext <- defaultALContext
    

    refs <- GlobalRefs
        <$> Font.loadFont (cfgFont config) [32 .. 128] 128
        <*> loadSheetFromPng "assets/images/spritesheet.png"
        <*> return alContext
        <*> loadPacSounds alContext (cfgSounds config)
        <*> newIORef levels
        <*> return (cfgLabels config)
        <*> return Map.empty
        <*> newIORef Nothing
        <*> newIORef False
        <*> newTimer

    flip runReaderT alContext $
        setMute False

        -- Strg + Q

    -- print (config :: GlobalConfig)
    runGameLoop $ do
        evalStateT loadLevel refs
    mainLoop
