module Strategies where

import Data.Foldable
import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.Vector as Vec

import Types
import Navigation
import Lattice ( left, right, spiral, up, posFlip )
import NeuralNet


phaseify :: PurePolicy -> PurePolicy
phaseify policy environment = if phase environment then
                                ({-restrict .-} policy) environment
                              else
                                (flip . {-restrict .-} policy) (environment . posFlip)
  where
    coordFlip (x, y) = (-x, y)
    flip F = F
    flip L = R
    flip R = L
    restrict F = F 
    restrict L = L
    restrict R = F

strictPhaseify :: PurePolicy -> PurePolicy
strictPhaseify policy environment = if phase environment then
                                (restrict . policy) environment
                              else
                                (flip . restrict . policy) (environment . posFlip)
  where
    coordFlip (x, y) = (-x, y)
    flip F = F
    flip L = R
    flip R = L
    restrict F = F 
    restrict L = L
    restrict R = L

phase :: Board -> Bool
phase env = even (distToRight+distToTop)
  where
    distToTop   = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\y -> env (0 :|: y)) [1..])
    distToRight = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\x -> env (x :|: 0)) [1..])

perceiver :: (Board -> Vec.Vector CellState) -> DNN -> PurePolicy
perceiver perceive dnn = run dnn . perceive

surrounding :: Int -> Board -> Vec.Vector CellState
surrounding = flip fmap . Vec.fromList . spiral

fromPureP :: PurePolicy -> Policy
fromPureP purePolicy = actionToDist . purePolicy

actionToDist a b | a == b = 1
actionToDist _ _ = 0

quickNext :: Board -> Board
quickNext env (0 :|: 0) = Wall 
quickNext env pos    = env pos

argmin :: (Foldable t, Ord a) => (b -> a) -> t b -> b
argmin f = minimumBy (comparing f)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:as) = Just a

goTowards :: Position -> Action
goTowards (x :|: y)
  | y > abs x = F
  | x > 0 = R
  | otherwise = L

findClosest :: CellState -> Board -> Maybe Position
findClosest cstate board = find ((cstate ==). board) (spiral (2*searchRange))

isSnake :: CellState -> Bool
isSnake (Snek _ _) = True
isSnake _ = False

{-reasonable :: PurePolicy
reasonable env
  | env up    == Appel = F
  | env right == Appel = R
  | env left  == Appel = L
  | env up    == Clear = F
  | env left  == Clear = L
  | env right == Clear = R
reasonable _ = R-}

avoider :: PurePolicy
avoider env | env up    == Clear = F
  | env left  == Clear = L
  | env right == Clear = R
avoider _ = F
{-monadicFinder :: PurePolicy
monadicFinder env = safeAction env (maybe (avoider env) goTowards nextPathPos)
  where 
    nextPathPos = do --itsa monad
      pos <- findClosest Appel env
      safeHead (getPath env (0, 0) pos)-}

{-tailFinder :: PurePolicy
tailFinder env = fst (argmin snd [(action, getPositionValue (relDir action)) | action <- actions])
  where
  getPositionValue pos | (not.isFree.env) pos = infty      
                       | null pathToTail  = 10000 --there are not enough numbers to embedd better orderings
                       | length pathToTail < tailLengthLag = 10000 - (fromIntegral.length) pathToTail
                       | env pos == Appel = -infty
                       | null pathToAppel = 1/(fromIntegral.length) pathToTail
                       | otherwise        = -(1/ (fromIntegral.length) pathToAppel)
    where
    [pathToAppel, pathToTail] = getPaths (quickNext env) pos [const (Appel ==), isSafeTail]
    score = (round.ttl.env) (0, 0)
    
    isSafeTail stepsPassed (Snek _ n) = stepsPassed > n+1
    isSafeTail _           _          = False

    entireTailLength = fromMaybe 0 (tailLength env (0, 0))
    
    tailLengthLag = score - entireTailLength

    --isSafeTail' stepsPassed (Snek _ n) = stepsPassed > n+tailLengthLag +1 --tailLengthLag actually depends on part of tail
    --isSafeTail' _           _          = False

restrictedTailFinder :: PurePolicy
restrictedTailFinder env = fst (argmin snd [(action, getPositionValue (relDir action)) | action <- {-actions-}[F, phase]])
  where
  distToTop   = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\y -> env (0, y)) [1..])
  distToRight = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\x -> env (x, 0)) [1..])
  phase = if even (distToRight+distToTop)
    then L
    else R

  getPositionValue pos | (not.isFree.env) pos = infty      
                       | null pathToTail  = 10000 --there are not enough numbers to embed better orderings
                       | length pathToTail < tailLengthLag = 10000 - (fromIntegral.length) pathToTail
                       | env pos == Appel = -infty
                       | null pathToAppel = 1/(fromIntegral.length) pathToTail
                       | otherwise        = -(1/ (fromIntegral.length) pathToAppel)
    where
    [pathToAppel, pathToTail] = getPaths (quickNext env) pos [const (Appel ==), isSafeTail]
    score = (round.ttl.env) (0, 0)
    
    isSafeTail stepsPassed (Snek _ n) = stepsPassed > n
    isSafeTail _           _          = False

    entireTailLength = fromMaybe 0 (tailLength env (0, 0))
    
    tailLengthLag = score - entireTailLength


tailLength :: Board -> Position -> Maybe Int --untested
tailLength env pos = case env pos of
  Snek _id n -> Just $ 1+ fromMaybe 0 (tailLength env =<< preceedingTailPart pos)
    where
      preceedsCurr (Snek _id' m) 
        | _id' == _id && (m==n-1 || m==n-2) = True
      preceedsCurr _                            = False

      preceedingTailPart pos = find (preceedsCurr.env) (neumannNeighbors pos)
  _            -> Nothing



randomer :: Policy
randomer board = maybe arbitrary (actionToDist . goTowards) (findClosest Appel board)

arbitrary :: Action -> Float
arbitrary _ = 1

safeRandAction :: Board -> Distribution -> Distribution
safeRandAction board distri action | board (relDir action) == Clear = distri action
                                   | otherwise                      = 0

safeAction :: Board -> Action -> Action
safeAction env action | env (relDir action) == Clear = action
                      | otherwise                    = reasonable env

harvester :: PurePolicy
harvester env = safeAction env (maybe F goTowards (findClosest Appel env))-}



