module Config where

screenRadius :: Float
screenRadius = 300 --350 also works
frameRate :: Int
frameRate = 20
parallelWorkers :: Int -- should be set to #cores - 1
parallelWorkers = 7