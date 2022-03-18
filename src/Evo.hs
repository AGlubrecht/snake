{-# LANGUAGE FlexibleInstances #-}

module Evo where

import Control.Monad.State         ( (<=<) )
import Control.Parallel.Strategies ( Strategy, parListChunk, rdeepseq )

import Config                      ( parallelWorkers )
import Random                      ( Contingent, getRandom, getRandomR, getShuffled )



class Evolving a where
    mutate :: [Float] -> a -> Contingent a

type Mutation g = g -> Contingent g

instance Evolving Float where
  mutate [rate] x = getRandomR (x-rate, x+rate)
  mutate _ _ = error "tried to mutate Float with multiple rates"


instance (Traversable t, Evolving a) => Evolving (t a) where
  mutate (rate:rates) = mapM (\element -> do
    randF <- getRandomR (0, 1)
    
    if randF < rate then do --monadic bernoulliPick
      mutate rates element
      
    else do
      return element)
  
  mutate [] = error " Not enough mutation rates supplied for an Evolving Traversible"


--like evoStep2 but without crossover
evoStep :: Mutation a -> [a] -> [Float] -> Contingent [a]
evoStep mutator genoms fitnesses = mapM mutator offspring
  where
    normFitnesses = map (max 0) fitnesses
    reproduction_factor = fromIntegral (length genoms) / sum normFitnesses
    proportions = zip ((roundWell . map  (reproduction_factor * )) normFitnesses) genoms
    offspring = concatMap (uncurry replicate) proportions 



evoStep2 :: ((a, a) -> Contingent a)
         ->  (a -> Contingent a)
         ->  [a] -> [Float] -> Contingent [a]
evoStep2 crossoverF mutationF genoms fitnesses = 
    mapM   (mutationF <=< crossoverF) 
  . uncurry zip 
  . splitAt batchSize <=< getShuffled
  $ uncurry replicate =<< proportions

  where
    proportions         = zip ((roundWell . map  (2*reproduction_factor * )) fitnesses) genoms --proportions sum up to 2 * batchSize
    reproduction_factor = fromIntegral batchSize / sum fitnesses
    batchSize           = length fitnesses

  {-multiply the organisms roughly proportional to their fitness, match them up randomly,
    apply crossover and mutation-}


--rounds s.t. rd(sum a) = sum (map rd a)
roundWell :: (RealFrac a) => [a] -> [Int]
roundWell as = zipWith (-) accums (0:accums)
  where accums = (map round . scanl1 (+)) as




        