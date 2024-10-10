{-# LANGUAGE LambdaCase #-}

module Game.Map.Loading 
  ( loadConfig
  , makeGameState
  ) where

import Data.Aeson (eitherDecodeStrict)
import Data.Array (Array, (!))
import qualified Data.Array as Array
import qualified Data.ByteString as ByteString
import Data.Map (Map)
import qualified Data.Map as Map

import Game.Enums
import Game.Pacman
import Game.Ghost
import Game.Map.Chart
import Game.Map.Dijkstra
import Game.Map.Config

import Game.State 
import Control.Monad.IO.Class (MonadIO, liftIO)

import Debug.Trace


type Items = Map Cell Item

loadConfig :: MonadIO m => FilePath -> m LevelConfig
loadConfig file = do
    content <- liftIO $ ByteString.readFile file
    case eitherDecodeStrict content of 
        Left err     -> liftIO $ fail err
        Right config -> return config


makeCharts :: [ChartConfig] -> Charts
makeCharts configs = charts
  where 
    n = length configs
    charts = Array.listArray (1, n) $ map makeChart configs

    makeChart :: ChartConfig -> Chart
    makeChart config =
        Chart 
          { chartId     = cfgId config
          , chartCells  = cells
          , chartOrigin = (cfgX config, cfgY config)
          } 
      where
        m = cfgNumCells config
        cells = Array.listArray ((1, 1), (m, m)) 
                . map makeCell 
                . Array.assocs 
                . cfgCells $ config

        makeCell :: ((Int, Int), CellType) -> Cell
        makeCell ((i, j), _type) = 
            Cell
              { cellId        = (cfgId config, i, j)
              , cellNeigh     = neighbour
              , cellBlocked   = _type == WALL -- TODO
              , cellType      = _type
              , cellOrigin    = origin
              , cellRelCenter = relCenter
              , cellSize      = size
              , cellDistance  = seq dijkstraResult distance
              } 
          where  
            size   = 1 / fromIntegral m
            origin = 
              ( cfgX config +     fromIntegral (j-1) * size
              , cfgY config + 1 - fromIntegral  i    * size
              )
            relCenter = 
              ( fst origin + 0.5 * size - cfgX config
              , snd origin + 0.5 * size - cfgY config
              )

            dijkstraResult = runDijkstra configs (cfgId config) relCenter 1  -- TODO m

            distance :: Cell -> Float
            distance other = queryDistance dijkstraResult ix (cellRelCenter other)
              where (ix, _, _) = cellId other
      
            neighbour :: Direction -> Cell
            neighbour = \case
                LEFT  -> if j == 1 
                            then chartCells (charts ! cfgLeft config) ! (i, m)
                            else cells ! (i, j - 1)
                RIGHT -> if j == m 
                            then chartCells (charts ! cfgRight config) ! (i, 1)
                            else cells ! (i, j + 1)
                UP    -> if i == 1 
                            then chartCells (charts ! cfgUp config) ! (m, j)
                            else cells ! (i - 1, j)    
                DOWN  -> if i == m 
                            then chartCells (charts ! cfgDown config) ! (1, j)
                            else cells ! (i + 1, j)  

makePacman :: Charts -> PacmanConfig -> Pacman 
makePacman charts config = 
    Pacman
      { pacCell          = chartCells (charts ! c) ! (i, j)
      , pacDirection     = cfgPacDirection config
      , pacCenter        = (0, 0)
      , pacSpeed         = cfgPacSpeed config
      , pacNextDirection = cfgPacDirection config
      , pacNumEaten      = 0
      }
  where 
    Position c i j = cfgPacStart config -- TODO Position -> Cell
  
makeGhost :: Charts -> GhostPhase -> GhostConfig -> Ghost
makeGhost charts phase config =
    Ghost
      { ghostId         = cfgGhostName config
      , ghostCell       = chartCells (charts ! startC) ! (startI, startJ)
      , ghostDirection  = cfgGhostDirection config
      , ghostCenter     = (0, 0)
      , ghostMode       = phaseMode phase -- cfgGhostMode config
      , ghostSpeed      = cfgGhostSpeed config
      , ghostTargetTile = chartCells (charts ! homeC) ! (homeI, homeJ)
      , ghostHomeTile   = chartCells (charts ! homeC) ! (homeI, homeJ)
      , ghostRetreating = 0
      }
  where 
    Position startC startI startJ = cfgGhostStart config -- TODO Position -> Cell
    Position homeC  homeI  homeJ  = cfgGhostHome config

makeItems :: Charts -> Items
makeItems = Map.fromList . foldl func [] . cellList
  where 
    func acc cell = case cellType cell of 
            ITEM item -> (cell, item) : acc
            _         -> acc

makeGameState :: LevelConfig -> GameState
makeGameState config = 
    GameState
      { gsPacman = makePacman charts (cfgPacman config)
      , gsGhosts = map (makeGhost charts (head (cfgPhases config))) (cfgGhosts config) 
      , gsCharts = charts
      , gsItems  = makeItems charts
      , gsScore  = 0
      , gsLifes  = 3
      , gsPhases = cfgPhases config
      , gsTime   = 0
      , gsConfig = config
      }
  where 
    charts = makeCharts $ cfgCharts config


