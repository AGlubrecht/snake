{-# LANGUAGE LambdaCase #-}

module Plot where

import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss

import Control.Concurrent ( MVar, ThreadId, modifyMVar_, swapMVar, forkIO, newMVar )

import Util ( len )

{- PLOTTING -}

class Monoid a => Plotable a where
  draw :: a -> Picture

instance Real x => Plotable [x] where
  draw list = line $ zip
    (map (/(len+1)) [0..])
    (map (/max) list')
    where 
      len = realToFrac (length list) :: Float
      max = realToFrac (maximum list) ::Float
      list' = map realToFrac list :: [Float]

runningAvg :: Float -> [Float] -> [Float]
runningAvg inertia = reverse . go . reverse 
  where 
  go [] = []
  go [a] = [a]
  go (a:as) = let b:bs = go as in
              (a + b*inertia) / (1 + inertia) : b : bs

avgWidget :: Float -> Plot [Float] -> Point -> Point -> Widget [Float]
avgWidget inertia (Plot as) (x, y) (w, h) = Widget
  mempty
  (\log -> do
    newElems <- swapMVar as mempty
    return $ log <> newElems)
  (\log -> translate x y . scale w h . graphLines [black, red] $ [0:log, 0:runningAvg inertia log])
  (const return)

plotToWidget :: Plotable a => Plot a -> Point -> Point -> Widget a
plotToWidget (Plot a) (x, y) (w, h) = Widget
  mempty
  (\log -> do
    newElems <- swapMVar a mempty
    return $ log <> newElems)
  (translate x y . scale w h . draw)
  (const return)

graphLines :: [Color] -> [[Float]] -> Picture
graphLines colors lists = 
  if null lists || any null lists 
  then error "graphLines: empty list" 
  else Pictures $ zipWith drawList colors lists
  where
    max = maximum $ maximum <$> lists
    drawList _color as = color _color $ line $ zip
      ((<$> [0..]) (/maximum (len <$> lists)))
      ((<$> as) (/max))

newtype Plot a = Plot {content :: MVar a}

plotter :: Plotable a => Plot a -> IO ()
plotter (Plot var) = playIO
  (InWindow "Window" (1000, 1000) (10, 10))
  white
  1
  mempty
  (return . scale 200 200 . draw)
  (const return)
  (const $ (<$> swapMVar var mempty) . flip (<>))

(+=) :: Plotable a => Plot a -> a -> IO ()
(Plot var) += a = modifyMVar_ var (return . (<> a))

new :: Plotable a => IO (Plot a)
new = Plot <$> newMVar mempty

newPlot :: Plotable a => IO (Plot a)
newPlot = do
  plot <- Plot <$> newMVar mempty
  forkIO (plotter plot)
  return plot

updateList :: MVar [Float] -> [Float] -> IO [Float]   
updateList channel list = (list ++) <$> swapMVar channel []


{- CONFIGURATION -}

data Configs = Configs {
  strength :: Int,
  mRates    :: [Float],
  gameLength :: Int,
  snakesPerGame :: Int,
  boardSize :: Int,
  fitnessPressure :: Float
} deriving (Show, Read)


confWidget :: MVar Configs -> Widget Configs
confWidget confVar = Widget
  (Configs 0 [1, 2, 3, 4, 5] 0 0 4 0)
  return
  (const $ Pictures [])
  (\case
  (EventKey (Char '+' ) Down _ _) -> (\configs -> do
    let configs' = configs{ strength = strength configs + 1 }
    swapMVar confVar configs'
    return configs')
  (EventKey (Char 'r' ) Down _ _) -> const (do
    contents <- readFile "configs.txt"
    let configs' = read contents
    print configs'
    swapMVar confVar configs'
    return configs')
  _ -> return)
   

{- WIDGET -}

data Widget a = Widget {
  _initial :: a,
  _update :: a -> IO a,
  _draw :: a -> Picture,
  _onEvent :: Event -> a -> IO a 
}

(<:>) :: Widget a -> Widget b -> Widget (a, b)
(Widget a_initial a_update a_draw a_onEvent) 
  <:> (Widget b_initial b_update b_draw b_onEvent) = Widget
  (a_initial, b_initial)

  (\(a,b) -> do
    a' <- a_update a
    b' <- b_update b
    return (a', b') 
  )

  (\(a, b) -> Pictures [a_draw a, b_draw b])

  (\e (a,b) -> do
    a' <- a_onEvent e a
    b' <- b_onEvent e b
    return (a', b') 
  )

guiFromWidget :: Widget a -> IO ThreadId
guiFromWidget (Widget _initial _update _draw _onEvent) = forkIO $ playIO 
  (InWindow "Window" (1000, 1000) (10, 10))
  white
  20
  _initial
  (return . _draw)
  _onEvent
  (const _update)

