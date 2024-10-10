module Game.Map.Chart 
 ( ChartPlan(..)
 , Chart(..)
 , Charts
 , Cell(..) 
 , CellType(..)
 , isCellIdContained
 , cellList
 ) where  

import Data.Array as Array
import qualified Data.Map as Map

import Game.Enums

-- Datentyp: Kartenplan, ein Quadrat ist eine Karte. Die Verklebungen werden durch planNeigh bestimmt.
-- Aus dem planArray wird das Labyrinth erstellt. 'X' steht für blockierte Wege, 'f' steht Position der Frucht,
-- 'o' steht für die Reversefrüchte. Der planOrigin gibt die Lage der Karte im Fenster an.
data ChartPlan = ChartPlan 
  { planId      :: !Int
  , planNeigh   :: !(Array Direction Int)
  , planArray   :: !(Array (Int, Int) Char)
  , planOrigin  :: !(Float, Float)
  } deriving (Eq, Show)

-- Datentyp: Karte, enthält Information über Nachbarkarten, Einteilung in Zellen und Ursprung der Karte (links unten). 
data Chart = Chart 
  { chartId     :: !Int
  --, chartNeigh  :: Direction -> Chart
  , chartCells  :: !(Array (Int, Int) Cell)
  , chartOrigin :: !(Float, Float)
  } deriving (Eq, Show)
  
type Charts = Array Int Chart

data CellType = EMPTY | WALL | ITEM Item
    deriving (Eq, Show)

-- Datentyp: Zellen, die Id enthält Information über die Karte und die Position der Zellen.
-- Angabe über Nachbarzelle, ob Zellen blockiert sind, relativer Ursprung der Zellen und die Größe.
data Cell = Cell 
  { cellId        :: !(Int, Int, Int)
  , cellNeigh     :: !(Direction -> Cell)
  , cellBlocked   :: !Bool
  , cellType      :: !CellType
  , cellOrigin    :: !(Float, Float)
  , cellRelCenter :: !(Float, Float) -- center of the cell relative to the chart origin
  , cellSize      :: !Float
  , cellDistance  :: Cell -> Float -- TODO: lazy evaluation?
  } 

instance Show Cell where 
    show = show . cellId

instance Eq Cell where
  (==) a b = (cellId a) == (cellId b)

instance Ord Cell where
  compare a b = compare (cellId a) (cellId b)

-- Erhalte aus allen Karten alle Zellenlisten.
cellList :: Charts -> [Cell]
cellList = concatMap (Array.elems . chartCells) . Array.elems

-- Schau, ob bestimmte Zelle in Zellliste enthalten ist. Für Items wichtig.
isCellIdContained :: [(Int, Int, Int)] -> Cell -> Bool
isCellIdContained l cell = elem (cellId cell) l




