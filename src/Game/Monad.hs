{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

module Game.Monad 
 ( MonadGame(..)
 , loopStarting
 , clearAllSound
 , LevelRefs(..)
 , lensState
 , lensStartState
 ) where


import Animation
import Game.State
import Game.Enums (Item(..), Direction, itemScore, oppositeDirection, Sound(..))
import Game.Map.Config
import Control.Lens (Lens', use, lens, assign, modifying)
import Control.Monad.State (MonadState)

import qualified Data.Map as Map
import Game.Ghost
    ( chooseTargetTile,
      lensRetreating,
      moveGhost,
      Ghost(ghostMode, ghostCell, ghostRetreating) )
import Game.Pacman
import Game.Ghost (MonadRandom)
import Game.Entity (Entity(..))
import Control.Monad (when, forM, forM_, unless)

data LevelRefs = LevelRefs
  { lvlState        :: !GameState   -- modified
  , lvlStartState   :: !GameState   -- modified
  } deriving (Eq, Show)

lensState :: Lens' LevelRefs GameState
lensState = lens lvlState $ \refs state -> refs { lvlState = state }

lensStartState :: Lens' LevelRefs GameState
lensStartState = lens lvlStartState $ \refs state -> refs { lvlStartState = state }


class (MonadState LevelRefs m, MonadRandom m) => MonadGame m where

    display :: LevelPhase -> Int -> m ()

    inputDirection :: m (Maybe Direction)

    getDeltaTime :: m Float

    playSound :: Sound -> m ()
    stopSound :: Sound -> m ()

clearAllSound  :: MonadGame m => m ()
clearAllSound = forM_ [minBound .. maxBound] stopSound


isHit :: MonadGame m => m Bool
isHit = do
    pacman <- use $ lensState . lensPacman
    ghosts <- use $ lensState . lensGhosts
    let hitting ghost 
            | ghostRetreating ghost > 0   = False
            | ghostMode ghost == FRIGHTEN = False
            | otherwise = ghostCell ghost == pacCell pacman
    return $ any hitting ghosts
    


gameMoveEntities :: MonadGame m => Float -> m ()
gameMoveEntities deltaTime = do

    inputDirection >>= mapM_
        (assign (lensState . lensPacman . lensNextDirection))

    -- move pacman
    modifying (lensState . lensPacman) $
        movePacman deltaTime
    -- move ghosts
    state  <- use lensState

    let speedModifier ghost
            | ghostRetreating ghost > 0   = 3
            | ghostMode ghost == FRIGHTEN = 0.5
            | otherwise                   = 1

    assign (lensState . lensGhosts) =<< forM (gsGhosts state) (\ghost -> do
        moveGhost (deltaTime * speedModifier ghost) ghost)

    pacman <- use $ lensState . lensPacman

    modifying (lensState . lensGhosts . traverse) $
        chooseTargetTile pacman

gameEatItem :: MonadGame m => Item -> m ()
gameEatItem POINT = do
    n <- use $ lensState . lensPacman . lensNumEaten
    if even n
        then playSound EAT_POINT_1
        else playSound EAT_POINT_2
    modifying (lensState . lensPacman . lensNumEaten) (+1)

gameEatItem FRUIT =
    playSound EAT_FRUIT

gameEatItem REVERSE = do
    phase <- gsGhostPhase <$> use lensState
    time  <- cfgFrightenTime . cfgTiming . gsConfig <$> use lensState
    case phaseMode phase of
        FRIGHTEN ->
            modifying lensState $ modifyPhase $ \phase -> phase { phaseDuration = time } -- TODO
        _        -> do
            modifying lensState $ enterGhostPhase (GhostPhase FRIGHTEN time) -- TODO check retreating?
            modifying (lensState . lensGhosts . traverse . lensDirection) oppositeDirection -- TODO check retreating
            stopSound SIREN
            playSound FRIGHTENED

gameEatItems :: MonadGame m => m ()
gameEatItems = do
    pCell <- use $ lensState . lensPacman . lensCell
    items <- use $ lensState . lensItems
    forM_ (Map.lookup pCell items) $ \item -> do
        modifying (lensState . lensScore) (+ itemScore item)
        gameEatItem item
        modifying (lensState . lensItems) $ Map.delete pCell

numEatenGhosts :: GameState -> Int
numEatenGhosts = length . filter ((>0) . ghostRetreating) . gsGhosts

gameEatGhosts :: MonadGame m => m ()
gameEatGhosts = do
    pCell <- use $ lensState . lensPacman . lensCell
    nextMode <- phaseMode . gsGhostNextPhase <$> use lensState
    modifying (lensState . lensGhosts . traverse) $ \ghost ->
        if ghostMode ghost == FRIGHTEN && ghostCell ghost == pCell
            then ghost { ghostRetreating = 3, ghostMode = nextMode }
            else ghost

-- game animations

loopAnimStart :: MonadGame m => m ()
loopAnimStart = do
    playSound START
    display STARTING 4230
    playSound SIREN

loopAnimDying :: MonadGame m => m ()
loopAnimDying = do
    clearAllSound
    playSound DEATH_1
    forM_ [0..11] $ \counter -> do
        when (counter == 10 || counter == 11) $ do
            playSound DEATH_2
        display (DYING counter) 200
    display (DYING 12) 750

loopAnimRespawn :: MonadGame m => m ()
loopAnimRespawn = do
    display RESPAWN 2000
    playSound SIREN

loopAnimWinning :: MonadGame m => m ()
loopAnimWinning = do
    clearAllSound
    forM_ [0..19] $ \counter -> do
        display (WINNING counter) 200

loopStarting :: MonadGame m => m ()
loopStarting = do
    -- setup + monad state here
    state0 <- use lensState
    loopAnimStart
    loopLifes
    display GAME_OVER 0

loopLifes :: MonadGame m => m ()
loopLifes = do
    loopRunning
    display RUNNING 500
    loopAnimDying
    modifying (lensState . lensLifes) (+ (-1))
    lifes <- use $ lensState . lensLifes
    when (lifes > 0) $ do
        loopRespawn
        loopLifes

-- whileM_ :: Monad m => m Bool -> m () -> m ()
-- whileM_ condition monad = 
--     condition >>= \case
--         True  -> monad >> whileM_ condition monad
--         False -> return ()

-- Dinge, die geupdated werden.
loopRunning :: MonadGame m => m ()
loopRunning = do
    deltaTime <- getDeltaTime
    modifying (lensState . lensTime) (+ deltaTime)
    --liftIO . print . (*1000) $ deltaTime
    gameMoveEntities deltaTime

    gameEatItems
    oldEaten <- numEatenGhosts <$> use lensState
    gameEatGhosts
    newEaten <- numEatenGhosts <$> use lensState

    
    modifying (lensState . lensScore) (+ 200 * (newEaten - oldEaten))
    
    
    oldPhase <- phaseMode . gsGhostPhase <$> use lensState  -- TODO lens
    modifying lensState $ progressGhostPhase deltaTime
    newPhase <- phaseMode . gsGhostPhase <$> use lensState -- TODO lens
    when (oldPhase == FRIGHTEN && newPhase /= FRIGHTEN) $ do
        stopSound FRIGHTENED
        playSound SIREN
    
    modifying (lensState . lensGhosts . traverse . lensRetreating) $
        max 0 . (+ (-deltaTime))

    newEaten' <- numEatenGhosts <$> use lensState
    when (newEaten > 0 && newEaten' == 0) $
        stopSound RETREATING -- TODO sound bug?

    -- Überprüfe, ob das Spiel zu Ende ist. Ändere Gameoverreferenz und führe den GameOverLoop an.
 
    when (newEaten > oldEaten) $ do
        playSound EAT_GHOST
        display EATING 1000
        when (oldEaten == 0) $
            playSound RETREATING

    items <- use $ lensState . lensItems
    when (Map.null items) $ do
        display RUNNING 0
        loopWinning

    display RUNNING 0

    hit <- isHit
    unless hit $ do
        loopRunning

loopRespawn :: MonadGame m => m ()
loopRespawn = do
    startState <- use lensStartState
    modifying lensState $ \state ->
        startState
          { gsItems = gsItems state
          , gsScore = gsScore state
          , gsLifes = gsLifes state
          }
    loopAnimRespawn

loopWinning :: MonadGame m => m ()
loopWinning = do
    loopAnimWinning
    modifying (lensStartState . lensGhosts . traverse . lensSpeed) (* 1.5)
    oldtstate <- use lensState
    modifying lensStartState $ \state ->
        state
          { gsScore = gsScore oldtstate
          , gsLifes = gsLifes oldtstate
          }
    use lensStartState >>= assign lensState
    loopAnimRespawn
    -- TODO correct animation?