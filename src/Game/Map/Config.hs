{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Game.Map.Config 
  ( LevelConfig(..)
  , PacmanConfig(..)
  , GhostConfig(..)
  , ChartConfig(..)
  , TimingConfig(..)
  , cfgChartNeighId
  , cfgNumCells
  , Position(..)
  , GhostPhase(..)
  , GhostName(..)
  , GhostMode(..)
  ) where

import Game.Enums
import Game.Ghost
import Game.Map.Chart

import Control.Monad (unless, forM)

import Data.Aeson 
  ( FromJSON(..)
  , withObject
  , withText
  , withArray
  , (.:)
  , Value
  )
import Data.Aeson.Types (Parser)

import qualified Data.Text as Text

import Data.Array (Array)
import qualified Data.Array as Array

import Data.Vector (toList)
import Data.String (String)

-- TODO validate Position and Id

data GhostPhase = GhostPhase  
  { phaseMode     :: !GhostMode
  , phaseDuration :: !Float 
  }
  deriving (Eq, Show)

data LevelConfig = LevelConfig
  { cfgName   :: String
  , cfgPacman :: PacmanConfig
  , cfgGhosts :: [GhostConfig]
  , cfgCharts :: [ChartConfig]
  , cfgPhases :: [GhostPhase]
  , cfgTiming :: TimingConfig
  } deriving (Eq, Show)

instance FromJSON LevelConfig where 
    parseJSON = withObject "level" $ \obj -> 
        LevelConfig
            <$> obj .: "name"
            <*> obj .: "pacman"
            <*> obj .: "ghosts"
            <*> obj .: "charts"
            <*> obj .: "phases"
            <*> obj .: "timing"


data PacmanConfig = PacmanConfig
  { cfgPacSpeed     :: Float
  , cfgPacDirection :: Direction
  , cfgPacStart     :: Position
  } deriving (Eq, Show)

instance FromJSON PacmanConfig where 
    parseJSON :: Value -> Parser PacmanConfig
    parseJSON = withObject "pacman" $ \obj -> 
        PacmanConfig
            <$> obj .: "speed"
            <*> obj .: "direction"
            <*> obj .: "start"


data GhostConfig = GhostConfig
  { cfgGhostName      :: GhostName
  , cfgGhostSpeed     :: Float
  , cfgGhostDirection :: Direction
  , cfgGhostStart     :: Position
  , cfgGhostHome      :: Position
  } deriving (Eq, Show)

instance FromJSON GhostConfig where 
    parseJSON = withObject "ghost" $ \obj -> 
        GhostConfig
            <$> obj .: "name"
            <*> obj .: "speed"
            <*> obj .: "direction"
            <*> obj .: "start"
            <*> obj .: "home"

data TimingConfig = TimingConfig
  { cfgFrightenTime :: Float 
  , cfgDotBlink     :: Float
  } deriving (Eq, Show)

instance FromJSON TimingConfig where 
    parseJSON = withObject "timing" $ \obj -> 
        TimingConfig
            <$> obj .: "frighten"
            <*> obj .: "dot-blink"



data ChartConfig = ChartConfig
  { cfgId    :: Int
  , cfgLeft  :: Int
  , cfgRight :: Int
  , cfgUp    :: Int
  , cfgDown  :: Int
  , cfgX     :: Float
  , cfgY     :: Float
  , cfgCells :: Array (Int, Int) CellType
  } deriving (Eq, Show)

cfgChartNeighId :: ChartConfig -> Direction -> Int -- Array Direction Int
cfgChartNeighId chart = \case
    LEFT  -> cfgLeft chart
    RIGHT -> cfgRight chart
    UP    -> cfgUp chart
    DOWN  -> cfgDown chart

cfgNumCells :: ChartConfig -> Int 
cfgNumCells = fst . snd . Array.bounds . cfgCells

instance FromJSON ChartConfig where 
    parseJSON = withObject "chart" $ \obj -> 
        ChartConfig
            <$> obj .: "id" 
            <*> obj .: "left" 
            <*> obj .: "right"
            <*> obj .: "up"
            <*> obj .: "down"
            <*> obj .: "x"
            <*> obj .: "y"
            <*> (obj .: "cells" >>= parseCells)
            

parseCellType :: Char -> Parser CellType
parseCellType symbol = 
    case symbol of 
      ' ' -> return EMPTY
      'X' -> return WALL
      '.' -> return $ ITEM POINT
      'o' -> return $ ITEM REVERSE
      'f' -> return $ ITEM FRUIT
      _   -> fail $ "parsing cell failed, got " ++ show symbol 

parseCells :: Value -> Parser (Array (Int, Int) CellType)
parseCells = 
    withArray "cells" $ \array -> do
        rows <- forM (toList array) $ withText "row" (return . Text.unpack)
        let n = length rows
        unless (all ((== n) . length) rows) $
            fail "parsing row failed, not a square"
        Array.listArray ((1, 1), (n, n)) <$> mapM parseCellType (concat rows)


data Position = Position 
  { posChart     :: Int
  , posRow       :: Int
  , posColumn    :: Int
  } deriving (Show, Eq) --, Ord, Enum, Bounded, Ix)

instance FromJSON Position where 
    parseJSON = withObject "position" $ \obj -> 
        Position
            <$> obj .: "chart" 
            <*> obj .: "row" 
            <*> obj .: "column"


instance FromJSON Direction where 
    parseJSON = withText "direction" $ \text ->
        case text of
            "left"  -> return LEFT
            "right" -> return RIGHT
            "up"    -> return UP
            "down"  -> return DOWN
            _       -> fail $ "parsing direction failed, got " ++ show text

instance FromJSON GhostName where 
    parseJSON = withText "ghost name" $ \text -> 
        case text of
            "blinky" -> return BLINKY
            "inky"   -> return INKY
            "pinky"  -> return PINKY
            "clyde"  -> return CLYDE
            _        -> fail $ "parsing ghost name failed, got " ++ show text

instance FromJSON GhostMode where 
    parseJSON = withText "ghost mode" $ \text -> 
        case text of
            "scatter"  -> return SCATTER
            "frighten" -> return FRIGHTEN
            "chase"    -> return CHASE
            _          -> fail $ "parsing ghost mode failed, got " ++ show text
            

instance FromJSON GhostPhase where 
    parseJSON = withObject "ghost phase" $ \obj -> 
        GhostPhase
            <$> obj .: "mode" 
            <*> obj .: "duration" 

