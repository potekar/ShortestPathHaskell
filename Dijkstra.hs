{-# LANGUAGE DeriveGeneric #-}
module Dijkstra where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)


type Graph = Map.Map Int [(Int, Int)]  -- Map from node to list of (neighbor, weight)
type Distance = Map.Map Int Int        -- Map from node to distance
type Previous = Map.Map Int Int        -- Map from node to previous node

data DijkstraStep = DijkstraStep
  { current :: Int
  , visited :: [Int]
  , distances :: [(Int, Int)]
  } deriving (Show, Generic)

-- instance ToJSON DijkstraStep

-- New: Dijkstra with steps
-- Returns (final distances, previous, steps)
dijkstraWithSteps :: Graph -> Int -> Maybe Int -> (Distance, Previous, [DijkstraStep])
dijkstraWithSteps graph startNode endNode = dijkstraSteps' graph startNode endNode (Map.singleton startNode 0) Map.empty Set.empty []

-- Helper for dijkstraWithSteps
-- Accumulates steps as it goes
dijkstraSteps' :: Graph -> Int -> Maybe Int -> Distance -> Previous -> Set.Set Int -> [DijkstraStep] -> (Distance, Previous, [DijkstraStep])
dijkstraSteps' graph current endNode distances previous visited steps
    | Set.member current visited = (distances, previous, steps)
    | case endNode of
        Just end -> Set.member end visited
        Nothing -> False = (distances, previous, steps)
    | otherwise = dijkstraSteps' graph nextNode endNode newDistances newPrevious newVisited (steps ++ [step])
    where
        currentDist = fromMaybe maxBound (Map.lookup current distances)
        neighbors = fromMaybe [] (Map.lookup current graph)
        (newDistances, newPrevious) = updateNeighbors neighbors current currentDist distances previous
        newVisited = Set.insert current visited
        nextNode = findNextNode newDistances newVisited
        step = DijkstraStep
            { current = current
            , visited = Set.toList newVisited
            , distances = Map.toList newDistances
            }

-- Original Dijkstra (for compatibility)
dijkstra :: Graph -> Int -> (Distance, Previous)
dijkstra graph start = dijkstra' graph start (Map.singleton start 0) Map.empty Set.empty

dijkstra' :: Graph -> Int -> Distance -> Previous -> Set.Set Int -> (Distance, Previous)
dijkstra' graph current distances previous visited
    | Set.member current visited = (distances, previous)
    | otherwise = dijkstra' graph nextNode newDistances newPrevious newVisited
    where
        currentDist = fromMaybe maxBound (Map.lookup current distances)
        neighbors = fromMaybe [] (Map.lookup current graph)
        (newDistances, newPrevious) = updateNeighbors neighbors current currentDist distances previous
        newVisited = Set.insert current visited
        nextNode = findNextNode newDistances newVisited

updateNeighbors :: [(Int, Int)] -> Int -> Int -> Distance -> Previous -> (Distance, Previous)
updateNeighbors [] _ _ dist prev = (dist, prev)
updateNeighbors ((node, weight):rest) current currentDist distances previous
    | newDist < oldDist = updateNeighbors rest current currentDist 
        (Map.insert node newDist distances)
        (Map.insert node current previous)
    | otherwise = updateNeighbors rest current currentDist distances previous
    where
        oldDist = fromMaybe maxBound (Map.lookup node distances)
        newDist = currentDist + weight

findNextNode :: Distance -> Set.Set Int -> Int
findNextNode distances visited = 
    fst $ Map.foldrWithKey findMin (0, maxBound) distances
    where
        findMin node dist (minNode, minDist)
            | Set.member node visited = (minNode, minDist)
            | dist < minDist = (node, dist)
            | otherwise = (minNode, minDist)

-- Helper function to get the shortest path
getShortestPath :: Previous -> Int -> Int -> [Int]
getShortestPath previous start end = reverse $ getPath end
    where
        getPath node
            | node == start = [start]
            | otherwise = node : getPath (fromMaybe start (Map.lookup node previous))

-- Example graph
exampleGraph :: Graph
exampleGraph = Map.fromList [
    (1, [(2, 7), (3, 9), (6, 14)]),
    (2, [(1, 7), (3, 10), (4, 15)]),
    (3, [(1, 9), (2, 10), (4, 11), (6, 2)]),
    (4, [(2, 15), (3, 11), (5, 6)]),
    (5, [(4, 6), (6, 9)]),
    (6, [(1, 14), (3, 2), (5, 9)])
    ] 