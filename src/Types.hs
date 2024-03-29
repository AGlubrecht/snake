module Types where

--This could all be moved to where it is needed

import Data.Array ( Array )
import Data.Sequence ( Seq )
import qualified Data.Vector as V 



type ArrBoard  = Array Position CellState
type ArrRule   = ArrBoard -> ArrBoard

infix 3 :|:
data Position  = Int :|: Int deriving (Eq, Ord)

data CellState = Snek Int Int | SnakeHead Int Int | Clear | Appel Int | Wall deriving (Eq, Show)
data Action    = L | R | F deriving (Eq, Show)
data Status    = Dead | Alive deriving (Eq, Show)

type Board     = Position ->  CellState
type Rule      = Board -> Board
type Combinator a = a -> a -> a

type TotRule   = CellState -> Int -> CellState
type Policy    = Board -> Action -> Float
type PurePolicy = Board -> Action
type ActionPicker = (Action -> Float) -> Action
type Distribution = Action -> Float
type TargetTest = Int -> CellState -> Bool


data Game = Game {
  arrBoard :: ArrBoard,
  players :: [Player],
  apples :: V.Vector Position,
  rPositions :: [Position],
  rPicks :: [Float],
  clearcount :: Int
}

data Player = Player {
  _id  :: Int,
  score :: Int,
  policy :: Policy,
  snake :: Seq Position,
  direction :: Position,
  status :: Status,
  age :: Int
}

data Optional a = Custom a | Default

actions :: [Action]
actions = [L, R, F]

