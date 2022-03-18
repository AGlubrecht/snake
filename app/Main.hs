module Main where

import Graphics.Gloss.Interface.IO.Interact

import Types
import Strategies
import Lattice
import Config
import Display
import System.Random
import qualified Data.Sequence as S
import Runner

main :: IO ()
main = r1 --unner --animateGame testGame

{-testGame :: Game
testGame = Game
  (toArr evRange (\pos -> if pos == (0, 0) then Appel else Clear))
  [
    --Player 1 3 (fromPureP monadicFinder) up up 0 0,
    --Player green  3 (fromPureP restrictedTailFinder) left right,
    --Player yellow 3 (fromPureP restrictedTailFinder) down down,
    Player 2 3 (fromPureP restrictedTailFinder) (S.replicate  3 right) right Alive 0
  ]
  (randCoords $ mkStdGen 1)
  (randPicks $ mkStdGen 2)-}