{-# LANGUAGE LambdaCase #-}

module Display where


import qualified Data.Vector   as Vec
import qualified Data.Sequence as Seq

import Control.Monad      ( forever )
import Data.Foldable      ( Foldable(toList) )
import Data.Array         ( (!), assocs, bounds )
import Control.Concurrent ( MVar, swapMVar, forkIO, newMVar, readMVar, threadDelay )

import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game


import Types
    ( ArrBoard,
      CellState(..),
      Game(apples, players, arrBoard),
      Player(status, score, snake),
      Position(..),
      Status(Alive) )


import Util    ( fat1, mapPair )
import Config  ( frameRate )
import Lattice ( ttl, toPair )
import Game    ( emptyGame, gameStep )
import Plot    ( Widget(Widget) )


bufferedAnimation :: IO Game -> IO ()
bufferedAnimation contingentGame = do
  initialGame <- contingentGame
  frameVar <- newMVar initialGame

  workerThread <- forkIO (computeFrames frameVar)
  animateFrames frameVar
  where 
    computeFrames frameVar = forever $ do
      --putStrLn "computing frames..." 
      frame <- readMVar frameVar
      --threadDelay 100000
      let frame' = gameStep frame
      swapMVar frameVar (seq (ttl (arrBoard frame' ! (0 :|: 0))) frame')

animateFrames :: MVar Game -> IO ()
animateFrames frameVar = playIO
  windowDisplay
  white
  frameRate
  initialModel
  drawingFunc
  inputHandler
  updateFunc
  where
    initialModel :: MVar Game
    initialModel = frameVar

    drawingFunc :: MVar Game -> IO Picture
    drawingFunc frameVar = do
      frame <- readMVar frameVar
      --putMVar frameVar frame
      --(return . drawArrBoard . arrBoard) frame
      return (scale 380 380 (drawGame frame))

    inputHandler :: Event -> MVar Game -> IO (MVar Game)
    inputHandler _ = return

    updateFunc ::  Float -> MVar Game -> IO (MVar Game)
    updateFunc _ = return

    windowDisplay :: Display
    windowDisplay = InWindow "Window" (1000, 1000) (10, 10)


{- GAME -}

gameToWidget :: MVar Game -> Float -> Widget Game
gameToWidget gameVar gameSize = Widget
  emptyGame
  (return . gameStep)
  (scale gameSize gameSize . drawGame)
  (\case
    (EventKey (SpecialKey KeyUp) Down _ _) -> const (readMVar gameVar)
    _                                      -> return)


drawGame :: Game -> Picture
drawGame game = Pictures [
    drawBorder,
    drawBackGround,
    drawAppels, 
    drawHeads,
    drawSnake
  ]
  where
    drawBorder     = color (greyN 0.5) . translate (-0.5 * cellSize) 0 $ rectangleSolid (2+2*cellSize) (2+2*cellSize)
    drawBackGround = color blue        . translate (-0.5 * cellSize) 0 $ rectangleSolid  2              2

    drawAppels     = color red . Pictures . map (\pos -> drawCell cellSize pos (Appel 0)) . Vec.toList . apples $ game

    drawHeads = Pictures
      [drawCell cellSize (Seq.index (snake player) 0) (SnakeHead (score player) 1) 
                                  | player <- players game, status player == Alive]
    
    drawSnake = color green  . Pictures 
      . map (thickLine (cellSize/2) . map (toCoords cellSize) . toList . snake) . players $ game

    cellSize = 1 / (fromIntegral.snd.toPair.snd.bounds.arrBoard) game



{- BOARD -}

drawArrBoard :: Float -> ArrBoard -> Picture
drawArrBoard cellSize = Pictures . map (uncurry (drawCell cellSize)) . assocs

drawCell :: Float -> Position -> CellState -> Picture
drawCell cellSize (x :|: y) state  = translate 
  (fromIntegral x * cellSize) 
  (fromIntegral y * cellSize) 
  (color 
    (colorOf state) 
    (rectangleUpperSolid cellSize cellSize)
  )

thickLine :: Float -> Path -> Picture
thickLine width path = Pictures $ zipWith (drawPiece width) path (tail path)

--draws a piece of the snakes body
drawPiece :: Float -> (Float, Float) -> (Float, Float) -> Picture
drawPiece width (x1, y1) (x2, y2) = translate minx miny (rectangleUpperSolid dx dy)
  where
    minx = min x1 x2 + fat1 (y1 == y2) * width --fat1 means if y1 == y2 then width else 0
    miny = min y1 y2 -width/2
    dx   = abs (x1-x2) +width 
    dy   = abs (y1-y2) +width

colorOf :: CellState -> Color
colorOf Clear           = blue
colorOf (Appel _)       = red
colorOf Wall            = greyN 0.5
colorOf (SnakeHead i _) = intToColor i
colorOf (Snek col _)    = blue--col



{- DYNAMIC GRAPH -}

drawList :: Point -> Point -> [Float] -> Picture 
drawList corner (w, h) list = 
  if   null list 
  then error "drawList: empty List" 
  else uncurry translate corner $ line (zip xs ys) 
  where xs = map (((w/).fromIntegral.length) list *) [0..]
        ys = map (h/ maximum list * ) list

updateList :: MVar [Float] -> [Float] -> IO [Float]   
updateList channel list = (list ++) <$> swapMVar channel []
--takes new elements out of the channel (isn't ++the wrong way around?)



{- GENERAL -}

intToColor :: Int -> Color
intToColor n = makeColorI ((n*63)  `mod` 255)
                          ((n*127) `mod` 255)
                          ((n*255) `mod` 255)
                          255

toCoords :: Float -> Position -> Point
toCoords cellSize pos = (x, y+cellSize/2) 
  where 
    (x, y) = mapPair ((cellSize*) . fromIntegral) (toPair pos)


