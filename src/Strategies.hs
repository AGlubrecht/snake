module Strategies where

import Data.Foldable
import Data.List

import Data.Maybe
import qualified Data.Vector as Vec

import Types
import Navigation
import Lattice
import NeuralNet

import Util
import Debug.Trace


{- GENERAL -}

perceiver :: (Board -> Vec.Vector CellState) -> DNN -> PurePolicy
perceiver perceive dnn = run dnn . perceive

surrounding :: Int -> Board -> Vec.Vector CellState
surrounding = flip fmap . Vec.fromList . spiral

fromPureP :: PurePolicy -> Policy
fromPureP  = (actionToDist .)

actionToDist :: Action -> Action -> Float
actionToDist = fat1 ..< (==)

quickNext :: Board -> Board --weird name
quickNext env (0 :|: 0) = Wall 
quickNext env pos    = env pos



{- PHASES -}

phaseify :: PurePolicy -> PurePolicy
phaseify policy environment = if   phase environment 
                              then policy environment
                              else (flip . policy) (environment . posFlip)
  where
    flip F = F
    flip L = R
    flip R = L
    restrict F = F 
    restrict L = L
    restrict R = F

strictPhaseify :: PurePolicy -> PurePolicy
strictPhaseify policy environment = if   phase environment 
                                    then (restrict . policy) environment
                                    else (flip . restrict . policy) (environment . posFlip)
  where
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

  


{- TAIL-FINDING -}

{-restrictedTailFinder :: PurePolicy
restrictedTailFinder env = fst (argmin snd [(action, getPositionValue (relDir action)) | action <- {-actions-} [F, phase]])
  where
  distToTop   = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\y -> env (0 :|: y)) [1..])
  distToRight = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\x -> env (x :|: 0)) [1..])
  phase = if even (distToRight+distToTop)
    then L
    else R

  getPositionValue pos | (not.isFree.env) pos = infty      
                       | otherwise        = -(1/ len pathToAppel)
    where
    [pathToAppel, pathToTail] = getPaths (quickNext env) pos [const isAppel, isSafeTail]
    score = (ttl.env) (0 :|: 0)
    
    isSafeTail stepsPassed (Snek _ n) = stepsPassed > n
    isSafeTail _           _          = False

    entireTailLength = fromMaybe 0 (tailLength env (0 :|: 0))
    
    tailLengthLag = score - entireTailLength-}

restrictedTailFinder :: PurePolicy
restrictedTailFinder env = fst (traceShowId $ argmin snd ([(action, getPositionValue (relDir action)) | action <- [F, phase]]))
  where
  distToTop   = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\y -> env (0 :|: y)) [1..])
  distToRight = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\x -> env (x :|: 0)) [1..])
  phase = if even distToTop-- +distToRight)
    then L
    else R

  getPositionValue pos | (not.isFree.env) pos = infty+1
                       | null safePathToTail = infty - 1 - len pathToTail
                       | isAppel (env pos) = 0
                       | null pathToAppel = (infty / 2) - len pathToTail
                       | otherwise        = len pathToAppel + 1 / len pathToTail 
    where
    [pathToAppel, pathToTail, safePathToTail] = getPhasePaths (quickNext env) pos [const (isAppel .), const isTailEndAdjacent, \n -> (isSafeTail n .)]
    score = (ttl.env) (0 :|: 0)

    isSafeTail stepsPassed (Snek _ n) = stepsPassed > n
    isSafeTail _           _          = False


tailFinder :: PurePolicy
tailFinder env = fst (traceShowId $ argmin snd ([(action, getPositionValue (relDir action)) | action <- actions]))
  where
  distToTop   = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\y -> env (0 :|: y)) [1..])
  distToRight = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\x -> env (x :|: 0)) [1..])
  phase = if even (distToRight+distToTop)
    then L
    else R

  getPositionValue pos | (not.isFree.env) pos = infty+1 -- There's something happening here, but I don't know what it is :(      
                       | null pathToTail  = infty --apparently, trace doesn't work right in local functions. This is the best option often, which doesn't mak any sense
                       | null safePathToTail = infty - len pathToTail
                       | isAppel (env pos) = 0
                       | null pathToAppel = (infty / 2) - len pathToTail
                       | otherwise        = len pathToAppel + 1 / len pathToTail 
    where
    [pathToAppel, pathToTail, safePathToTail] = getPaths (quickNext env) pos [const isAppel, const isTail, isSafeTail]
    score = (ttl.env) (0 :|: 0)

    isSafeTail stepsPassed (Snek _ n) = stepsPassed > n
    isSafeTail _           _          = False



rTailFinder :: Policy
rTailFinder env = (1 / len candidates *) . fat1 . (`elem` candidates)
  where
    candidates = map fst (argmins snd ([(action, getPositionValue (relDir action)) | action <- actions]))
    distToTop   = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\y -> env (0 :|: y)) [1..])
    distToRight = fromMaybe (error "unbound grid") $ elemIndex Wall (map (\x -> env (x :|: 0)) [1..])
    phase = if even (distToRight+distToTop)
      then L
      else R

    getPositionValue pos | (not.isFree.env) pos = infty+1 -- There's something happening here, but I don't know what it is :(      
                        | null pathToTail  = infty --apparently, trace doesn't work right in local functions. This is the best option often, which doesn't mak any sense
                        | null safePathToTail = infty - len pathToTail
                        | isAppel (env pos) = 0
                        | null pathToAppel = (infty / 2) - len pathToTail
                        | otherwise        = len pathToAppel -- + 1 / len pathToTail 
      where
      [pathToAppel, pathToTail, safePathToTail] = getPaths (quickNext env) pos [const isAppel, const isTail, isSafeTail]
      score = (ttl.env) (0 :|: 0)
      
      isSafeTail stepsPassed (Snek _ n) = stepsPassed >= n --safety margin
      isSafeTail _           _          = False


tailLength :: Board -> Position -> Maybe Int --untested
tailLength env pos = case env pos of
  Snek _id n -> Just $ 1+ fromMaybe 0 (tailLength env =<< preceedingTailPart pos)
    where
      preceedsCurr (Snek _id' m) 
        | _id' == _id && (m==n-1 || m==n-2) = True
      preceedsCurr _                            = False

      preceedingTailPart pos = find (preceedsCurr.env) (neumannNeighborhood pos)
  _            -> Nothing




