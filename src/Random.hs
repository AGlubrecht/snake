module Random where

import System.Random         ( StdGen, Random(randomR, random), newStdGen, randomIO, mkStdGen, randoms, split )
import Control.Monad.State   ( MonadState(state), evalState, State )
import System.Random.Shuffle ( shuffle' )


{- FUNDAMENTALS -}

type Contingent = State StdGen

getRandom :: Random a => Contingent a
getRandom = state random

getRandomR :: Random a => (a, a) -> Contingent a
getRandomR range = state (randomR range)

toIO :: Contingent a -> IO a
toIO conVal = evalState conVal <$> randomIO

cLifter :: IO (State StdGen c -> c)
cLifter = flip evalState <$> newStdGen

getLifter :: IO (Contingent a -> a)
getLifter = flip evalState <$> randomIO 


{- ENTANGLING -}

mapCP :: (a -> Contingent b) -> [a] -> Contingent [b]
mapCP f as = cloneC $ f <$> as

--uses the same seed for each item
cloneC :: [Contingent a] -> Contingent [a]
cloneC conVals = do 
  stdGen <- getRandom
  return $ map (`evalState` stdGen) conVals


{- PROBABILITY DISTRIBUTIONS -}

bernoulliPick :: Float -> (b, b) -> Contingent b
bernoulliPick p (a, b) = do
  randF <- getRandomR (0, 1)
  if randF < p then return a
               else return b

pickFromPair :: (a, a) -> Contingent a
pickFromPair (a1, a2) = (\bool -> if bool then a1 else a2) <$> getRandom

getShuffled :: [a] -> Contingent [a]
getShuffled as = shuffle' as (length as) <$> (getRandom :: Contingent StdGen)

instance Random StdGen where
  random motherGen = (mkStdGen (-randInt), motherGen')
    where (randInt, motherGen') = random motherGen

  randomR = error "rGens don't have a range lol"

instance Random r => Random [r] where
  random g = (randoms g', g'')
    where (g', g'') = split g
  
  randomR (lowerBounds, upperBounds) g = (evalState (sequence (getRandomR <$> zip lowerBounds upperBounds)) g', g'')
    where (g', g'') = random g 