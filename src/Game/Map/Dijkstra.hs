{-# LANGUAGE LambdaCase #-}
module Game.Map.Dijkstra 
  ( runDijkstra
  , queryDistance
  , euclideanDistance
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Array.ST
import Data.STRef
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

import Game.Map.Config 
  ( ChartConfig(..) 
  , cfgChartNeighId
  ) 
import Game.Enums

type Point = (Float, Float)

euclideanDistance :: Point -> Point -> Float
euclideanDistance (x, y) (x', y') = sqrt $ dx * dx + dy * dy
  where 
    dx = x - x'
    dy = y - y'

shift :: Direction -> Point -> Point
shift direction (x, y) = 
    case direction of 
        LEFT  -> (x + 1, y)
        RIGHT -> (x - 1, y)
        UP    -> (x, y - 1)
        DOWN  -> (x, y + 1)


type PriorityQueue s a = STRef s (Set a)

emptyQueue :: ST s (PriorityQueue s a)
emptyQueue = newSTRef Set.empty

isEmpty :: PriorityQueue s a -> ST s Bool
isEmpty queue = null <$> readSTRef queue

push :: Ord a => PriorityQueue s a -> a -> ST s ()
push queue item = modifySTRef' queue (Set.insert item)

popMin :: Ord a => PriorityQueue s a -> ST s a
popMin queue = do
    item <- Set.findMin <$> readSTRef queue
    modifySTRef' queue (Set.delete item)
    return item


data Node = Node 
  { nodeChartIx   :: !Int
  , nodeDirection :: !Direction
  , nodeOffset    :: !Int
  } deriving (Show, Eq, Ord, Bounded, Ix) 

coord :: Node -> Int -> Point
coord node k = 
    case nodeDirection node of
        LEFT  -> (    0, 1 - t)
        RIGHT -> (    1,     t)
        UP    -> (1 - t,     1)
        DOWN  -> (    t,     0)
  where 
    t = fromIntegral (nodeOffset node) / fromIntegral k

adjacent :: Int -> Int -> [Node]
adjacent chartIx k = do 
    direction <- [LEFT, RIGHT, UP, DOWN] 
    offset    <- [0..k]
    return $ Node 
      { nodeChartIx   = chartIx
      , nodeDirection = direction
      , nodeOffset    = offset
      }

whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ condition body = 
    condition >>= \case
        False -> return ()
        True  -> body >> whileM_ condition body

type DijkstraResult = (Int, Point, Int, Array Node Float)

runDijkstra :: [ChartConfig] -> Int -> Point -> Int -> DijkstraResult
runDijkstra charts startIx (x, y) k = seq distance' (startIx, (x, y), k, distance') -- TODO 
  where 
    distance' = runSTArray $ do
        let n = length charts
            chartsArray = Array.listArray (1, n) charts
        
        queue <- emptyQueue 

        let bounds = (Node 1 minBound 0, Node n maxBound k)
            infinity = read "Infinity" :: Float
        distance <- newArray bounds infinity

        forM_ (adjacent startIx k) $ \node -> do
            let t = euclideanDistance (coord node k) (x, y)
            push queue (t, node)

        whileM_ (not <$> isEmpty queue) $ do
            (t, node) <- popMin queue
            t' <- readArray distance node
            when (t < t') $ do 
                writeArray distance node t
                let direction = nodeDirection node
                    chart     = (Array.!) chartsArray (nodeChartIx node)
                    neighIx   = cfgChartNeighId chart direction
                    point     = shift direction (coord node k)
                forM_ (adjacent neighIx k) $ \node' -> do
                    let s = euclideanDistance point (coord node' k)
                    push queue (s + t, node')
        return distance
    

queryDistance :: DijkstraResult -> Int -> Point -> Float
queryDistance (startIx, startPoint, k, distance) ix point = 
        min dist $ minimum $ (flip map) (adjacent ix k) $ \node ->
            let s = euclideanDistance point (coord node k)
                t = (Array.!) distance node
            in s + t
    where 
        dist = if startIx == ix 
                then euclideanDistance startPoint point 
                else read "Infinity"

