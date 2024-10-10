module Game.State 
  ( GameState(..)
  , GhostPhase(..)
  , LevelPhase(..)
  , lensPacman
  , lensGhosts
  , lensItems
  , lensScore
  , lensLifes
  , lensTime
  , lensConfig
  , modifyPhase
  , gsGhostPhase
  , gsGhostNextPhase
  , enterGhostPhase
  , progressGhostPhase
  ) where


import Control.Lens (Lens', lens, over)
import Data.Array (Array)
import Data.Map (Map)

import Game.Pacman (Pacman)
import Game.Ghost (Ghost(..), GhostMode(..))
import Game.Map.Chart (Chart, Cell)
import Game.Map.Config (LevelConfig, GhostPhase(..))
import Game.Enums (Item)

type Items  = Map Cell Item

data LevelPhase = 
      RUNNING | STARTING | DYING Int | EATING | GAME_OVER | RESPAWN | WINNING Int
    deriving (Eq, Show) -- TODO move to other file

data GameState = GameState
  { gsPacman :: !Pacman
  , gsGhosts :: ![Ghost]
  , gsCharts :: !(Array Int Chart)
  , gsItems  :: !Items
  , gsScore  :: !Int
  , gsLifes  :: !Int
  , gsPhases :: ![GhostPhase]
  , gsTime   :: !Float
  , gsConfig :: !LevelConfig
  } deriving (Eq, Show)

lensPacman :: Lens' GameState Pacman
lensPacman = lens gsPacman $ \state pacman -> state { gsPacman = pacman }

lensGhosts :: Lens' GameState [Ghost]
lensGhosts = lens gsGhosts $ \state ghosts -> state { gsGhosts = ghosts }

lensItems :: Lens' GameState Items
lensItems = lens gsItems $ \state items -> state { gsItems = items }

lensScore :: Lens' GameState Int
lensScore = lens gsScore $ \state score -> state { gsScore = score }

lensLifes :: Lens' GameState Int
lensLifes = lens gsLifes $ \state lifes -> state { gsLifes = lifes }

lensTime :: Lens' GameState Float
lensTime = lens gsTime $ \state time -> state { gsTime = time }

lensConfig :: Lens' GameState LevelConfig
lensConfig = lens gsConfig $ \state config -> state { gsConfig = config }


gsGhostPhase :: GameState -> GhostPhase
gsGhostPhase state = 
    case gsPhases state of 
        (phase:_) -> phase
        _         -> GhostPhase CHASE (read "Infinity")
        

gsGhostNextPhase :: GameState -> GhostPhase
gsGhostNextPhase state = 
    case gsPhases state of 
        (_:phase:_) -> phase
        _           -> GhostPhase CHASE (read "Infinity")


modifyPhase :: (GhostPhase -> GhostPhase) -> GameState -> GameState
modifyPhase f state = 
    case gsPhases state of 
        []     -> state 
        (p:ps) -> state { gsPhases = f p : ps}

enterGhostPhase :: GhostPhase -> GameState -> GameState
enterGhostPhase phase state = 
    over (lensGhosts . traverse)  
        (\ghost -> ghost  { ghostMode = phaseMode (gsGhostPhase state') })
        state'
  where 
    state' = state { gsPhases = phase : gsPhases state }


progressGhostPhase :: Float -> GameState -> GameState
progressGhostPhase time state = -- TODO execState
    case gsPhases state of 
        [] -> state
        (GhostPhase mode durr : ps) -> 
          if durr > time 
            then state { gsPhases = GhostPhase mode (durr - time) : ps }
            else 
              let state' = progressGhostPhase (time - durr) $ -- TODO bug
                              state { gsPhases = ps }
              in over (lensGhosts . traverse)  
                  (\ghost -> ghost { ghostMode = phaseMode (gsGhostPhase state') })
                  state'


-- gsGameOver :: GameState -> Bool
-- gsGameOver = (== 0) . gsLifes

-- type MonadGame m = StateT GameState