{-# LANGUAGE InstanceSigs #-}

module Game.Pacman 
 ( Pacman(..)
  , movePacman
  , lensNextDirection
  , lensNumEaten
 ) where  

import Control.Lens (Lens', lens)
import Data.Functor.Identity (Identity(..))

import Game.Enums (Direction)
import Game.Entity (Entity(..), move)
import Game.Map.Chart (Cell(..))


-- Datentyp: Spieler, enthält die Info über seine Zelle, Richtung, nächste Richtung und
-- wo er sich in der Zelle befindet. 
data Pacman = Pacman
  { pacCell          :: !Cell
  , pacDirection     :: !Direction
  , pacCenter        :: !(Float, Float)
  , pacSpeed         :: !Float
  , pacNextDirection :: !Direction
  , pacNumEaten      :: !Int
  } deriving (Eq, Show)

-- Pacman gehört zur Entityklasse.
instance Entity Pacman where

    lensCell :: Lens' Pacman Cell
    lensCell = lens pacCell $ \pacman cell -> pacman { pacCell = cell }

    lensDirection :: Lens' Pacman Direction
    lensDirection = lens pacDirection $ \pacman dir -> pacman { pacDirection = dir}

    lensCenter :: Lens' Pacman (Float, Float)
    lensCenter = lens pacCenter $ \pacman center -> pacman { pacCenter = center }

    lensSpeed :: Lens' Pacman Float
    lensSpeed = lens pacSpeed $ \pacman speed -> pacman { pacSpeed = speed }
    

lensNextDirection :: Lens' Pacman Direction
lensNextDirection = lens pacNextDirection $ \pacman dir -> pacman { pacNextDirection = dir }

lensNumEaten :: Lens' Pacman Int 
lensNumEaten = lens pacNumEaten $ \pacman n -> pacman { pacNumEaten = n }

-- Schaue ob, Richtung blockiert wird. Wenn nicht wähle die nächste Richtung.
choosePacmanDirection :: Monad m => Pacman -> m Direction
choosePacmanDirection pacman =
    let blockednextneigh = cellBlocked $ cellNeigh (pacCell pacman) (pacNextDirection pacman)
    in
        if blockednextneigh then
            return $ pacDirection pacman
        else
            return $ pacNextDirection pacman

-- Beweg Spieler, via der Entity move Funktion.
movePacman :: Float -> Pacman -> Pacman
movePacman time pacman = 
    runIdentity $ move time choosePacmanDirection pacman