{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module UI (module UI) where

import Game.Enums (Direction (..))
import Game.Map.Config (LevelConfig)
import Game.Monad (LevelRefs)
import Game.State (LevelPhase (GAME_OVER))

data Event = Quit | Resize | Mute | Fullscreen | Back | Enter | Arrow Direction


class Monad m => MonadUI m where

    type DrawContext m
    type Preview m

    mkPreview :: LevelConfig -> m (Preview m)

    evalLevel :: [Preview m] -> m LevelRefs
    drawScene :: DrawContext m -> m ()
    uiToggleAudio :: m ()

    uiDrawLevel :: LevelRefs -> LevelPhase -> m ()
    uiDrawPreview :: Preview m -> m ()

    pollEvent :: m (Maybe Event)

    sleep :: Int -> m ()

sceneLevelRun :: MonadUI m => [Preview m] -> m ()
sceneLevelRun levels = do
    result <- evalLevel levels
    sceneLevelGameOver result levels

sceneLevelGameOver :: MonadUI m => LevelRefs -> [Preview m] -> m ()
sceneLevelGameOver refs levels = do 
    uiDrawLevel refs GAME_OVER
    eventloop
  where 
    lshift = (++) . tail <*> return . head  -- TODO partial
    rshift = (:) . last <*> init            -- TODO partial
    eventloop = do
        pollEvent >>= \case 
            Just (Arrow RIGHT) -> 
                sceneLevelPreview $ lshift levels
            Just (Arrow LEFT) ->  
                sceneLevelPreview $ rshift levels
            Just Resize -> 
                sceneLevelGameOver refs levels
            Just Mute -> do
                uiToggleAudio
                sceneLevelGameOver refs levels
            Just Enter  -> do 
                sceneLevelRun levels
            _     -> do
                sleep 32
                eventloop

sceneLevelPreview :: MonadUI m => [Preview m] -> m ()
sceneLevelPreview levels = do
    uiDrawPreview (head levels)
    eventloop
  where 
    lshift = (++) . tail <*> return . head  -- TODO partial
    rshift = (:) . last <*> init            -- TODO partial
    eventloop = do
        pollEvent >>= \case 
            Just (Arrow RIGHT) -> do
                sceneLevelPreview $ lshift levels
            Just (Arrow LEFT) -> do 
                sceneLevelPreview $ rshift levels
            Just Resize -> 
                sceneLevelPreview levels
            Just Mute -> do
                uiToggleAudio
                sceneLevelPreview levels
            Just Enter  -> do 
                sceneLevelRun levels
            _     -> do
                sleep 32
                eventloop

