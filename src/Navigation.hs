module Navigation where

import Control.Monad.State
import Data.Array
import qualified Data.Map      as Map
import qualified Data.Sequence as Seq
import qualified Data.Set      as Set
import Data.Foldable

import Lattice 
import Types

searchRange :: Int
searchRange = 20

data SearchState = SearchState
  {
    searchList :: [Position],
    parentOf :: Array Position (Maybe Position),
    arrBoard :: Array Position CellState,
    pathLength :: Int,
    targets :: [Maybe Position]
  }

getPaths :: Board -> Position -> [TargetTest] -> [[Position]]
getPaths board initialPosition targetTests = evalState
  (multiBFS initialPosition targetTests)
  (
    SearchState 
    [initialPosition] 
    (array (boundsOf searchRange) [(x:|:y, Nothing) | x <- [-searchRange.. searchRange-1],
                                                       y <- [-searchRange.. searchRange-1]])
    (toArr searchRange board) 
    0
    (map (const Nothing) targetTests)
  )

multiBFS :: Position -> [TargetTest] -> State SearchState [[Position]]
multiBFS initialPos targetTests = do
  SearchState searchList parentOf arrBoard pathLength targets <- get
  let generatePathTo Nothing = []
      generatePathTo (Just pos) = reverse $ pathFrom pos
      pathFrom pos = case parentOf!pos of
        Nothing      -> []
        Just prevPos -> pos:pathFrom prevPos

  if null searchList || (Nothing `notElem` targets) 
    then return (map generatePathTo targets)
    else do
        let isFree pos = case fromArr arrBoard pos of
              Snek _ n -> n < pathLength
              Wall       -> False
              _          -> True

            childrens = map (filter isFree . neumannNeighborhood) searchList

            searchList' = (uniqueify . concat) childrens

            getTargetPos (Nothing, targetTest) = find (targetTest pathLength . (arrBoard!)) searchList'
            getTargetPos (justPos, _)          = justPos

            targets' = zipWith (curry getTargetPos) targets targetTests

            parentOf' = parentOf // concat [zip children (repeat $ Just parent) 
                    | (children, parent) <- zip childrens searchList]
            
            arrBoard' = arrBoard // zip searchList (repeat Wall) --should be Snek ? (score+pathlength) Wall, but sneks don't know their own score

            pathLength' = pathLength+1

        put (SearchState searchList' parentOf' arrBoard' pathLength' targets')
        multiBFS initialPos targetTests

uniqueify :: Ord a => [a] -> [a]
uniqueify = Set.elems . Set.fromList

data SearchState2 = SearchState2
  {
    searchSet2 :: Set.Set Position,
    parentOf2 :: Array Position (Maybe Position),
    arrBoard2 :: Array Position CellState,
    pathLength2 :: Int
  }

getPath :: Board -> Position -> Position -> [Position]
getPath board initialPosition targetPosition = evalState
  (latticeBFS initialPosition targetPosition)
  (SearchState2 
    (Set.singleton initialPosition) 
    (array (boundsOf searchRange) [(x:|:y, Nothing) | x <- [-searchRange.. searchRange-1],
                                                y <- [-searchRange.. searchRange-1]]) --why is emppty array so hard to make?
    (toArr searchRange board) 
    0
  )
--toArr
latticeBFS :: Position -> Position -> State SearchState2 [Position]
latticeBFS initialPos targetPos = do
  SearchState2 searchSet parentOf arrBoard pathLength <- get
  if Set.null searchSet
    then return []
    else do
      if parentOf!targetPos == Nothing 
      then do
        let isFree pos = case fromArr arrBoard pos of
              Snek _ n -> n < pathLength
              Wall       -> False
              _          -> True

            parents = Set.elems searchSet

            childrens = map (filter isFree . neumannNeighborhood) parents

            searchSet' = (Set.fromList . concat) childrens

            parentOf' = parentOf // concat [zip children (repeat $ Just parent) 
                    | (children, parent) <- zip childrens parents]
            
            arrBoard' = arrBoard // zip parents (repeat Wall) --should be Snek ? (score+pathlength) Wall, but sneks don't know their own score
            
            pathLength' = pathLength+1

        put (SearchState2 searchSet' parentOf' arrBoard' pathLength')
        latticeBFS initialPos targetPos
      else do
        let pathFrom pos = case parentOf!pos of
              Nothing       -> []
              Just prevPos -> pos:pathFrom prevPos 
        return (reverse $ pathFrom targetPos)

{-getShortestPath :: Board -> Position -> Position -> [Position]
getShortestPath board initialPosition targetPosition = evalState
  (bfs board initialPosition targetPosition)                                           --operation on state
  (BFSState (Seq.singleton initialPosition) (Set.singleton initialPosition) Map.empty) --starting sate

data BFSState = BFSState
  { bfsSearchQueue :: Seq.Seq Position
  , bfsVisistedPositions :: Set.Set Position
  , bfsParents :: Map.Map Position Position
  }

bfs :: Board -> Position -> Position -> State BFSState [Position]
bfs board initialPosition targetPosition = do
  BFSState searchQueue visitedSet parentsMap <- get
  if Seq.null searchQueue
    then return [] --no path was found
    else do
      let nextLoc = Seq.index searchQueue 0
      if nextLoc == targetPosition  --path found :)
        then return (unwindPath parentsMap [targetPosition])
        else do  --do a step of bfs
          let adjacentCells = getAdjacentPositions board nextLoc
              unvisitedNextCells = filter (\l -> not (Set.member l visitedSet)) adjacentCells
              newSearchQueue = foldr (flip (Seq.|>)) (Seq.drop 1 searchQueue) unvisitedNextCells --mabe modify bfs here and remove set
              newVisitedSet = Set.insert nextLoc visitedSet
              newParentsMap = foldr (\l -> Map.insert l nextLoc) parentsMap unvisitedNextCells
          put (BFSState newSearchQueue newVisitedSet newParentsMap)
          bfs board initialPosition targetPosition
  where
    unwindPath parentsMap currentPath = case Map.lookup (head currentPath) parentsMap of
      Nothing -> tail currentPath --reached beginning of path
      Just parent -> unwindPath parentsMap (parent : currentPath)

getAdjacentPositions :: Board -> Position -> [Position]
getAdjacentPositions board position = filter (isFree . board) (neumannNeighbors position)-}

