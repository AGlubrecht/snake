module Strategies where

import Data.Foldable
import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.Vector as Vec

import Types
import Navigation
import Lattice
import NeuralNet

import Util
import Debug.Trace


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
fromPureP  = (actionToDist .)

actionToDist :: Action -> Action -> Float
actionToDist = fat1 ..< (==)

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

restrictedTailFinder :: PurePolicy
restrictedTailFinder env = fst (argmin snd [(action, getPositionValue (relDir action)) | action <- {-actions-} [F, phase]])
  where
  distToTop   = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\y -> env (0 :|: y)) [1..])
  distToRight = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\x -> env (x :|: 0)) [1..])
  phase = if even (distToRight+distToTop)
    then L
    else R

  getPositionValue pos | (not.isFree.env) pos = infty      
                       {-| null pathToTail  = 10000 --there are not enough numbers to embed better orderings
                       | length pathToTail < tailLengthLag = 10000 - len pathToTail
                       | isAppel (env pos) = -infty
                       | null pathToAppel = 1/len pathToTail-}
                       | otherwise        = -(1/ len pathToAppel)
    where
    [pathToAppel, pathToTail] = getPaths (quickNext env) pos [const isAppel, isSafeTail]
    score = (ttl.env) (0 :|: 0)
    
    isSafeTail stepsPassed (Snek _ n) = stepsPassed > n
    isSafeTail _           _          = False

    entireTailLength = fromMaybe 0 (tailLength env (0 :|: 0))
    
    tailLengthLag = score - entireTailLength

tailFinder :: PurePolicy
tailFinder env = fst (argmin snd (traceShowId [(action, getPositionValue (relDir action)) | action <- actions]))
  where
  distToTop   = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\y -> env (0 :|: y)) [1..])
  distToRight = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\x -> env (x :|: 0)) [1..])
  phase = if even (distToRight+distToTop)
    then L
    else R

  getPositionValue pos | (not.isFree.env) pos = infty+1 -- There's something happening here, but I don't know what it is :(      
                       | null pathToTail  = trace ":) " infty --apparently, trace doesn't work right in local functions. This is the best option often, which doesn't mak any sense
                       | isAppel (env pos) = 0
                       | null pathToAppel = (infty / 2) - len pathToTail
                       | otherwise        = len pathToAppel
    where
    [pathToAppel, pathToTail] = getPaths (quickNext env) pos [const isAppel, isSafeTail]
    score = (ttl.env) (0 :|: 0)
    
    isSafeTail stepsPassed (Snek _ n) = True--stepsPassed > n + 1 --safety margin
    isSafeTail _           _          = False

    entireTailLength = fromMaybe 0 (tailLength env (0 :|: 0))
    
    tailLengthLag = score - entireTailLength





tailLength :: Board -> Position -> Maybe Int --untested
tailLength env pos = case env pos of
  Snek _id n -> Just $ 1+ fromMaybe 0 (tailLength env =<< preceedingTailPart pos)
    where
      preceedsCurr (Snek _id' m) 
        | _id' == _id && (m==n-1 || m==n-2) = True
      preceedsCurr _                            = False

      preceedingTailPart pos = find (preceedsCurr.env) (neumannNeighborhood pos)
  _            -> Nothing




