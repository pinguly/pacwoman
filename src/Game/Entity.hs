{-# LANGUAGE LambdaCase #-}

module Game.Entity 
 ( Entity(..)
 , move
 , isLeavingCell
 ) where  

import Game.Enums
import Game.Map.Chart
import Control.Lens (Lens', set, view)


class Entity a where 
    lensCell      :: Lens' a Cell
    lensDirection :: Lens' a Direction
    lensCenter    :: Lens' a (Float, Float)
    lensSpeed     :: Lens' a Float 

-- Bewege Character zum Zentrum
moveToCenter :: (Entity a, Monad m) => Float -> (a -> m Direction) -> a -> m a
moveToCenter speed chooseDirection entity = 
    let (x, y) = view lensCenter entity 
        norm = abs x  + abs y
    in
        if norm > speed then -- Bewege Entity Richtung Zentrum, indem das Zentrum verändert wird.
            return $ set lensCenter (x - speed * signum x, y - speed * signum y) entity
        else
            -- Bewege Entity im Zentrum.
            moveInCenter (speed - norm) chooseDirection $ set lensCenter (0, 0) entity

-- Bewegung im Zentrum. Dort wird die Richtung gewechselt.
moveInCenter :: (Entity a, Monad m) => Float -> (a -> m Direction) -> a -> m a
moveInCenter speed chooseDirection entity = do 
    direction <- chooseDirection entity
    let cell = view lensCell entity
    if cellBlocked $ cellNeigh cell direction then
        return entity
    else
        moveFromCenter speed chooseDirection $ set lensDirection direction entity

oppositeEdge :: Direction -> (Float, Float)
oppositeEdge = \case
    LEFT  -> (1, 0)
    RIGHT -> (-1, 0)
    UP    -> (0, -1)
    DOWN  -> (0, 1)

sgnX :: Direction -> Float
sgnX = \case
    LEFT  -> -1
    RIGHT -> 1
    _     -> 0

sgnY :: Direction -> Float
sgnY = \case
    UP   -> 1
    DOWN -> -1
    _    -> 0 

isLeavingCell :: Entity a => a -> Bool
isLeavingCell entity = 
    signum x == sgnX direction && signum y == sgnY direction
  where 
    (x, y) = view lensCenter entity
    direction = view lensDirection entity


-- Beweg die Entity vom Zentrum weg.
moveFromCenter :: (Entity a, Monad m) => Float -> (a -> m Direction) -> a -> m a
moveFromCenter speed chooseDirection entity =
    let (x, y) = view lensCenter entity
        norm = 1 - (abs x  + abs y)
        direction = view lensDirection entity
        neighcell = cellNeigh (view lensCell entity) direction
    in
        if norm > speed then
            return $ set lensCenter (x + speed * sgnX direction, y + speed * sgnY direction) entity
        else
            moveToCenter (speed - norm) chooseDirection $ 
                set lensCell neighcell $ set lensCenter (oppositeEdge direction) entity

-- Bewege Entity. Speed, 
move :: (Entity a, Monad m) => Float -> (a -> m Direction) -> a -> m a
move time chooseDirection entity = 
    let (x, y) = view lensCenter entity
        speed  = 2 * time * view lensSpeed entity -- TODO rename speed, 2?
        condition LEFT  = x > 0
        condition RIGHT = x < 0
        condition DOWN  = y > 0
        condition UP    = y < 0
    in
    if (x, y) == (0, 0) then
        moveInCenter speed chooseDirection entity
    else
        if condition (view lensDirection entity) then 
            moveToCenter speed chooseDirection entity
        else
            moveFromCenter speed chooseDirection entity