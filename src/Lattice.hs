module Lattice where

import System.Random ( Random(randomR, random) )
import Data.Array    ( Ix(inRange, range, index), bounds, (!), array, Array )
import Data.List     ( nub )


import Types
    ( CellState(Snek, Clear, Appel, Wall),
      ArrBoard,
      Board,
      Position(..),
      Action(..),
      ActionPicker )

import Util   ( infty, mapPair )
import Random ( getRandomR, Contingent )


{- POSITION -}

posFlip :: Position -> Position
posFlip (x :|: y) = (y :|: x) 

toPair :: Position -> (Int, Int)
toPair (x:|:y) = (x, y)

fromPair :: (Int, Int) -> Position
fromPair (x, y) = x:|:y

getRandomCoords :: Int -> Contingent [Position]
getRandomCoords boardSize = (getRandomR . unzip . repeat . boundsOf) (boardSize-1)

instance Random Position where
  random g = (rx:|:ry, g'')
    where
      (rx, g') = random g
      (ry, g'')= random g' 

  randomR (x1:|:y1, x2:|:y2) g = (rx:|:ry, g'')
    where
      (rx, g') = randomR (x1, x2) g
      (ry, g'')= randomR (y1, y2) g' 


instance Ix Position where
  range (p1, p2) = fromPair <$> range (toPair p2, toPair p2)
  index (p1, p2) p3 = index (toPair p1, toPair p2) (toPair p3)
  inRange (p1, p2) p3 = inRange (toPair p1, toPair p2) (toPair p3)


{- BOARD -}

toArr :: Int -> Board -> ArrBoard
toArr range board = array (boundsOf range) [
  (a :|: b, board (a :|: b)) | a <- [-range.. range-1],
                           b <- [-range.. range-1]]

fromArr :: ArrBoard -> Board
fromArr arr coords | isInBounds arr coords = arr!coords
fromArr _   _  = Wall

isInBounds :: Array Position e -> Position -> Bool
isInBounds arr (x :|: y) | minx < x && x < maxx && miny < y && y < maxy = True 
                      | otherwise                                    = False
  where
    (minx:|:miny, maxx:|:maxy) = bounds arr

boundsOf :: Int -> (Position, Position)
boundsOf range = (-range :|: -range , range-1 :|: range-1)


{- ACTION -}

toPick :: Float -> ActionPicker
toPick randFloat probabilityOf | randFloat < v1 = L
                               | randFloat > v2 = R
                               | otherwise = F
  where
    probSum = (sum . map probabilityOf) [L, R, F]
    v1 = probabilityOf L / probSum
    v2 = 1 - (probabilityOf R / probSum)


toEnv :: Position -> Position -> Position -> Position
toEnv (px:|:py) (dirX:|:dirY) (cx:|:cy) = (mx+px:|:my+py)
  where
    (mx:|: my) = case (dirX :|: dirY) of
              (1:|:0)  -> (cy:|: -cx)
              (-1:|:0) -> (-cy:|: cx)
              (0:|:1)  -> (cx:|: cy)
              (0 :|: -1) -> (-cx:|: -cy)
              (_:|: _)  -> undefined

toAbs :: Position  -> Action -> Position 
toAbs dir action = multP dir (relDir action)

addP  :: Position -> Position -> Position 
addP  (x1 :|: y1) (x2 :|: y2) = x1+x2 :|: y1+y2
multP :: Position -> Position -> Position
multP (x1 :|: y1) (x2 :|: y2) = x1*y2 + y1*x2 :|: - x1*x2 + y1*y2

up    :: Position
up    = 0 :|:1
down  :: Position
down  = 0 :|: -1
left  :: Position
left  = -1 :|: 0
right :: Position
right =  1 :|: 0

relDir :: Action -> Position
relDir L = -1 :|: 0
relDir R = 1  :|: 0
relDir F = 0  :|: 1


{- SURROUNDING -}

spirals :: [[Position]]
spirals = map spiral [0..]

spiral :: Int -> [Position]
spiral range = concat [ring n | n <- [0.. range]]

ring :: Int -> [Position]
ring 0 = [0 :|: 0]
ring n = concat [[i :|: n-i, n-i :|: -i, i-n :|: i, -i :|: i-n] | i <- [0.. n-1]]

mooreNeighborhood :: Position -> [Position]
mooreNeighborhood (x :|: y) = 
  [x-1 :|: y-1, x :|: y-1, x+1 :|: y-1,
   x-1 :|: y,              x+1 :|: y,
   x-1 :|: y+1, x :|: y+1, x+1 :|: y+1]

neumannNeighborhood :: Position -> [Position]
neumannNeighborhood (x :|: y) = 
  [             x :|: y-1,
   x-1 :|: y,               x+1 :|: y,
                x :|: y+1             ]


{- QUERIES -}

isFree :: CellState -> Bool 
isFree (Appel i) = True
isFree Clear = True 
isFree _     = False

isAppel :: CellState -> Bool 
isAppel (Appel _) = True 
isAppel _         = False

ttl :: CellState -> Double
ttl Wall       = infty
ttl (Snek _ n) = fromIntegral n
ttl _          = 0