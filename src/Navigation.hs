module Navigation where

import Control.Monad.State
import Data.Array
import qualified Data.Map      as Map
import qualified Data.Sequence as Seq
import qualified Data.Set      as Set
import Data.Foldable
import Data.Maybe
import Debug.Trace

import Lattice 
import Types
import Data.List (elemIndex)

searchRange :: Int
searchRange = 32

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
    0 --unsafety
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
        let isFree pos = pos /= (0 :|: 0) && ttl (fromArr arrBoard pos) <= pathLength

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

getPhasePaths :: Board -> Position -> [TargetTest] -> [[Position]]
getPhasePaths board initialPosition targetTests ={-} traceShow headDirection $ -}evalState
  (multiBFS2 neighborhood initialPosition targetTests)
  (
    SearchState 
    [initialPosition] 
    (array (boundsOf searchRange) [(x:|:y, Nothing) | x <- [-searchRange.. searchRange-1],
                                                      y <- [-searchRange.. searchRange-1]])
    (toArr searchRange board) 
    0 --unsafety
    (map (const Nothing) targetTests)
  )

  where
    distToTop   = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\y -> board (0 :|: y)) [1..])
    distToRight = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\x -> board (x :|: 0)) [1..])
    phase = if even (distToRight+distToTop) then L else R

    neighborhood (x :|: y) = [if even (distToRight - x) then (x :|: y+1) else (x :|: y-1),
                              if even (distToTop   - y) then (x-1 :|: y) else (x+1 :|: y)]

    headDirection = neighborhood (0 :|: 0)

multiBFS2 :: (Position -> [Position]) -> Position -> [TargetTest] -> State SearchState [[Position]]
multiBFS2 neighborhood initialPos targetTests = do
  SearchState searchList parentOf arrBoard pathLength targets <- get
  let generatePathTo Nothing = []
      generatePathTo (Just pos) = reverse $ pathFrom pos
      pathFrom pos = case parentOf!pos of
        Nothing      -> []
        Just prevPos -> pos:pathFrom prevPos

  if null searchList || (Nothing `notElem` targets) 
    then return (map generatePathTo targets)
    else do
        let isFree pos = pos /= (0 :|: 0) && ttl (fromArr arrBoard pos) <= pathLength

            childrens = map (filter isFree . neighborhood) searchList

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



