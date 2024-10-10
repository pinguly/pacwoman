{-# LANGUAGE LambdaCase #-}

module Game.Enums
  ( Direction(..)
  , oppositeDirection
  , Item(..)
  , itemScore
  , GhostName(..)
  , GhostMode(..)
  , Sound(..)
  ) where

import Data.Ix (Ix)


data Direction = LEFT | RIGHT | UP | DOWN
    deriving (Show, Eq, Ord, Enum, Bounded, Ix)

oppositeDirection :: Direction -> Direction
oppositeDirection = \case 
    LEFT  -> RIGHT 
    RIGHT -> LEFT
    UP    -> DOWN
    DOWN  -> UP


data Item = POINT | FRUIT | REVERSE
    deriving (Show, Eq, Ord, Enum, Bounded, Ix)

itemScore :: Num a => Item -> a
itemScore POINT   = 10 
itemScore REVERSE = 50
itemScore FRUIT   = 100


data GhostName = BLINKY | INKY | PINKY | CLYDE
    deriving (Show, Eq, Ord, Enum, Bounded, Ix)

data GhostMode = SCATTER | FRIGHTEN | CHASE
    deriving (Show, Eq, Ord, Enum, Bounded, Ix)


data Sound 
    = START 
    | SIREN
    | FRIGHTENED
    | RETREATING
    | DEATH_1
    | DEATH_2
    | EAT_POINT_1
    | EAT_POINT_2
    | EAT_FRUIT
    | EAT_GHOST
    deriving (Eq, Show, Ord, Enum, Bounded, Ix) -- TODO find fromEnum left