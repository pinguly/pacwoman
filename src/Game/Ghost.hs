{-# LANGUAGE InstanceSigs #-}

module Game.Ghost 
 ( Ghost(..)
 , GhostName(..)
 , GhostMode(..)
 , MonadRandom(..)
 , moveGhost
 , chooseTargetTile
 , lensRetreating
 ) where  

import Control.Lens (Lens', lens)

import Game.Entity (Entity(..), move)
import Game.Enums
import Game.Map.Chart
import Game.Pacman

-- Datentyp Geist, enthält dessen Zelle, Richtung, Zentrum, Zielzelle und Geistmodus.
data Ghost = Ghost
  { ghostId         :: !GhostName
  , ghostCell       :: !Cell
  , ghostDirection  :: !Direction
  , ghostCenter     :: !(Float, Float)
  , ghostSpeed      :: !Float
  , ghostMode       :: !GhostMode
  , ghostTargetTile :: !Cell
  , ghostHomeTile   :: !Cell
  , ghostRetreating :: !Float
  } deriving (Eq, Show)

instance Entity Ghost where

    lensCell :: Lens' Ghost Cell
    lensCell = lens ghostCell $ \ghost cell -> ghost { ghostCell = cell }

    lensDirection :: Lens' Ghost Direction
    lensDirection = lens ghostDirection $ \ghost dir -> ghost { ghostDirection = dir}

    lensCenter :: Lens' Ghost (Float, Float)
    lensCenter = lens ghostCenter $ \ghost center -> ghost { ghostCenter = center }

    lensSpeed :: Lens' Ghost Float
    lensSpeed = lens ghostSpeed $ \ghost speed -> ghost { ghostSpeed = speed }

lensRetreating :: Lens' Ghost Float
lensRetreating = lens ghostRetreating $ \ghost retr -> ghost { ghostRetreating = retr }

chooseTargetTile :: Pacman -> Ghost -> Ghost
chooseTargetTile pacman ghost = ghost { ghostTargetTile = targettile }
  where 
    paccell = pacCell pacman
    pacdirection = pacDirection pacman
    cellsize = cellSize paccell -- TODO use config
    targettile
        | ghostRetreating ghost > 0 = paccell
        | otherwise =
            case ghostId ghost of 
                BLINKY -> paccell 
                PINKY  -> cellNeigh (cellNeigh (cellNeigh (cellNeigh paccell pacdirection) pacdirection) pacdirection) pacdirection
                INKY   -> paccell -- TODO Herausfinden wie das geht
                CLYDE  -> if cellDistance (ghostCell ghost) paccell < 8 * cellsize 
                            then paccell  -- TODO 8
                            else ghostHomeTile ghost
                    

class Monad m => MonadRandom m where 
    random :: m Float -- uniform in [0, 1)

-- Geistrichtung wählen. Abhängig vom Geistmodus. Frighten zufällige Auswahl der Richtung.
chooseGhostDirection :: MonadRandom m => Ghost -> m Direction
chooseGhostDirection ghost
    | null nextdir        = 
        return $ oppositeDirection ghostdirection
    | length nextdir == 1 = 
        return $ head nextdir
    | ghostRetreating ghost > 0 = do
        let directions =  [ (cellDistance (ghostTargetTile ghost) cell, direction) | direction <- nextdir, let cell = cellNeigh ghostcell direction] 
        return $ snd $ maximum directions
    | otherwise = 
        case ghostMode ghost of 
            FRIGHTEN -> do
                r <- random 
                let index = floor $ r * fromIntegral (length nextdir) -- Wähle die Richtung zufällig.
                return $ nextdir !! index
            SCATTER -> do
                let directions = [ (cellDistance (ghostHomeTile ghost) cell, direction) | direction <- nextdir, let cell = cellNeigh ghostcell direction] 
                return $ snd $ minimum directions
            CHASE -> do
                let directions = [ (cellDistance (ghostTargetTile ghost) cell, direction) | direction <- nextdir, let cell = cellNeigh ghostcell direction]
                return $ snd $ minimum directions
  where
    ghostcell = ghostCell ghost
    ghostdirection = ghostDirection ghost
    dir = enumFrom LEFT
    nextdir = [direction | direction <- dir, not ( cellBlocked (cellNeigh ghostcell direction)), 
                            direction /= oppositeDirection ghostdirection]


-- Bewege den Geist.
moveGhost :: MonadRandom m => Float -> Ghost -> m Ghost
moveGhost time = 
    move time chooseGhostDirection