module Simulate where
import Types
import Game
import Lattice

simulate :: Int -> (Player -> Game -> Float) -> TotPolicy
simulate strength f =  undefined