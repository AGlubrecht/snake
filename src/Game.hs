module Game where


--is storing the apples unneccessary?


import Prelude hiding (reverse, replicate)

import qualified Data.Vector as Vec

import Debug.Trace

import System.Random     ( RandomGen(split) )
import Data.Array        ( Array, (!), (//), array, bounds )
import Data.Sequence     ( (<|), replicate, Seq((:<|), Empty, (:|>)) )
import Data.Foldable     ( Foldable(toList) )
import Data.List         ( nub )

import Types


import Util    ( asserting )
import Random  ( getRandomR, Contingent )
import Lattice ( getRandomCoords, up, addP, toAbs, fromArr, toArr, toPick, toEnv )



{- INITIALIZATION -}

createGame :: GameSettings -> [Policy] -> Contingent Game
createGame (GameSettings 
              settings
              (StartSettings appleCount startLength boardSize)) policies = do

  coords <- nub <$> getRandomCoords boardSize 
            `asserting` [(2*boardSize)^2 > appleCount + length policies]  
  let (apples, coords') = splitAt appleCount coords
  let heads             = take (length policies) coords'
  let startBoard = startB boardSize apples heads startLength

  appleCoords <- getRandomCoords boardSize
  actionPickers <- getRandomR (repeat 0.0, repeat 1.0) 

  let players = map (
        \(policy, _id, randPosition) -> 
          Player _id startLength policy (replicate startLength randPosition) up Alive 0) 
         (zip3  policies [0..] heads)
  return $ Game 
    startBoard 
    players 
    (Vec.fromList apples) 
    appleCoords
    actionPickers
    ((2*(boardSize))^2 - appleCount - (length policies * startLength)) --don't touch lol
    settings



startB :: Int -> [Position] -> [Position] -> Int -> ArrBoard 
startB boardSize applesPos headsPos startLength = emptyBoard // (apples ++ heads)
  where 
    emptyBoard = toArr (boardSize+1) (const Clear)
    apples = zip applesPos (map Appel [0..]) 
    heads = zip headsPos (map (SnakeHead startLength) [0..])

      
emptyGame :: Game 
emptyGame = Game
  (toArr 1 (const Clear))
  []
  Vec.empty
  []
  []
  0
  (IterationSettings 0 False 0)


{- ITERATION -}

gameStep :: Game -> Game
gameStep g = decrementGameLength(if clearcount g == 0 then g else foldl moveHead (g {players = []}) (players g)) -- reconstruct players

decrementGameLength :: Game -> Game 
decrementGameLength g = g {settings = (settings g) {gameLength = gameLength (settings g) - 1}}

moveHead :: Game -> Player -> Game
moveHead g@(Game arrBoard players apples rGen rPicks clearcount settings)
         p@(Player _id score policy _snake direction status age) =
  if clearcount == 0 then    g { players = p:players }
  else case _snake of
    Empty                 -> g { players = p:players }
    snakeInits :|> snakeEnd -> 
      case status of 
        Dead ->            decay
        Alive -> case fromArr arrBoard snakeHead' of
          Snek      _ _ -> decay
          SnakeHead _ _ -> decay
          Wall          -> decay
          Appel i       -> eat i
          _             -> survive
        where
          snakeHead :<| _ = _snake
          (snakeHead', direction') = apply (fromArr arrBoard) rPick snakeHead direction policy
          rCoord:rCoords = clearCoords arrBoard rGen
          rPick:rPicks' = rPicks
          snakeTailAssocs = (snakeEnd, Clear) : zip (toList  snakeInits) (map (Snek _id) [1..])

          decay = g{ 
            players = p{ status = Dead, snake = snakeInits }:players, 
            arrBoard = arrBoard // snakeTailAssocs,
            clearcount = clearcount+1
          }

          survive = g{
              players = p{
                snake = snakeHead' <| snakeInits,
                direction = direction',
                age = age+1
              }:players,
              rPicks = rPicks',
              arrBoard = arrBoard // ((snakeHead', SnakeHead _id score)
                                    :(snakeEnd, Clear)
                                    : snakeTailAssocs)
            }

          eat i = 
            let
              (persistingSnake, clearcount' , boardSnake                           ) = if growing settings then 
                (_snake,        clearcount-1, []                                 )   else
                (snakeInits,    clearcount  , (snakeEnd, Clear) : snakeTailAssocs)  
            in 
              g{
                players = p{
                  score = score+1,
                  age = age+1,
                  snake = snakeHead' <| persistingSnake,
                  direction = direction'
                }:players,

                apples = apples Vec.// [(i, rCoord)], 
                arrBoard = arrBoard // ((snakeHead', SnakeHead _id score):(rCoord, Appel i):boardSnake),
                rPositions = rCoords,
                rPicks = rPicks',
                clearcount = clearcount'
              }


apply :: Board -> Float -> Position -> Position -> Policy -> (Position, Position)
apply board picker pos direction policy = (pos', direction')
  where
    envBoard = board . toEnv pos direction
    direction' = (toAbs direction . toPick picker . policy) envBoard
    pos' = addP pos direction'


clearCoords :: ArrBoard -> [Position] -> [Position] --deletes all *leading* occupied positions from the list
clearCoords arrBoard (pos:poss) | arrBoard!pos == Clear = pos:poss
                                | otherwise             = clearCoords arrBoard poss
clearCoords _ [] = undefined


{- SCORING -}

scoreGame :: Game -> [Float]
scoreGame g = if   gameOver g 
              then map ((+ (-fitnessPressure (settings g))) . fromIntegral.  score) (players g)
              else scoreGame (gameStep g)

gameOver :: Game -> Bool
gameOver g = (not . (Alive `elem`) . map status .players) g
          || gameLength (settings g) == 0
          || ((clearcount g)) == 0