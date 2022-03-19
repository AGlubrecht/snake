module Runner where

--high-score: b4: 16 90%
--            b3: 11
--            b5: Median 44


--incorporate phases into ca model somehow
  --make phaseify a policy-transformer that restricts by taking the next best action
--extra completion reward? show completion rate
--automate config-adaptations, ff
--seperate nn params from activationfunc which stays constant
--solve almost-cycle

--show variance, change of population

--random snake starting directions
  --not yet an issue


--make DNNs storable with data.binary
--check what takes so long aka profiling
--random monad instead of contingent
--make Apple-randomness with Contingent-Monad (no?)


import Control.Concurrent.MVar
import Control.Concurrent
import Data.IORef
import Control.Parallel.Strategies
import Control.Monad.State
import Data.List.Split
import Data.Foldable

import Types
import NeuralNet
import Evo
import Strategies
import Lattice
import Display
import CA

import Plot
import Game
import Analytics
import Util
import Config
import Random
    ( Contingent, pickFromPair, toIO, cLifter, bernoulliPick )
import Data.Foldable (maximumBy)
import GHC.Base (VecElem(Int16ElemRep))





r1 :: IO ()
r1 = runEvolution
    (return $ zeroDNN reLU [dimension (surrounding 3 (const Clear)), 3, dimension L])
    (evoStep . mutate)
    (const $ fromPureP . phaseify . perceiver (surrounding 3))
    (createGame)
    (\gameLength -> map (fromIntegral . score) . players . flip (!!) gameLength . iterate gameStep)
    (`gameToWidget` screenRadius)
    dnnInfo
    7
    40

{- 
Mit entsprechendem Training findet der Perceiver auf Karten mit n <=5 zuverlässig einen Hamiltonkreis
Z.b. mit Rate [0.5, 0.01, 1] und einer dreiknotigen Zwischenebene
-}

{-r2 :: IO ()
r2 = runEvolution
    ({-randomDNN-}return $ zeroDNN reLU [cdim*2{-}, cdim + cdim `div` 2-}, cdim]) --initGenom
    (evoStep . mutate) --evoStep
    ((fromPureP . strictPhaseify) ..< runCA) --phenotype
    (flip createGame 1) ---
    (\gameLength -> map (fromIntegral . score) . players . flip (!!) gameLength . iterate gameStep)
    (`gameToWidget` screenRadius)
    dnnInfo
    7  --core_count 
    40--15 --games_per_core-}



runEvolution :: 
     Contingent genom -> ([Float] -> [genom] -> [Float] -> Contingent [genom])  
  -> (Int -> genom -> phenotype) -> (Int -> Int -> Int -> [phenotype] -> Contingent game) -> (Int -> game -> [Float])
  -> (MVar game -> Widget game) -> ([Float] -> [genom] -> String)
  -> Int -> Int -> IO ()

runEvolution
  randomGenom evoStep 
  phenotype createGame scoreGame 
  gameToWidget info
  core_count games_per_core = do

  genCounter <- newIORef 0
  cLift <- cLifter ---unneccessary

  configs <- newMVar (Configs 3 [0.5, 1, 1]  100 1 4 0 1 3 True) --actual start configurationt
  batchSize <- (core_count * games_per_core *) . snakesPerGame <$> readMVar configs 
  startGenoms <- toIO $ replicateM batchSize randomGenom
  
  gameVar <- newMVar =<< toIO (gameFromGenoms 1 3 4 2 startGenoms)
  scoreLog <- new
  --ageLog <- new
  guiFromWidget (translate 300 0 (gameToWidget gameVar) 
            <:> avgWidget 10 scoreLog (-700, -0) (300, 300) 
            -- <:> plotToWidget ageLog (-700, -600) (300, 300)
            <:> confWidget configs)
  print 0
  let evoLoop currentGenoms = do
        (Configs 
          strength 
          mRates 
          gameLength 
          snakes_per_game 
          boardSize 
          fitnessPressure 
          appleCount 
          startLength
          growing) <- readMVar configs
        let batchSize = core_count * games_per_core * snakes_per_game
        currGenCount <- readIORef genCounter
        --print 1
        games <- (toIO . mapM (gameFromGenoms appleCount startLength boardSize strength). chunksOf snakes_per_game) currentGenoms
        --print 2
        let gameResults = scoreGame gameLength <$> games
                        `using` parListChunk games_per_core rdeepseq

        swapMVar gameVar ((fst . argMax (maximum . snd)) (zip games gameResults))
        
        --print gameLength
        let fitnesses = concat gameResults
        --print fitnesses
        putStrLn $ "Generation: " ++  show currGenCount
              ++ info fitnesses currentGenoms

        --ageLog += [mean ages]
        scoreLog += [mean fitnesses]

        writeIORef genCounter (currGenCount+1)
        evoLoop =<< toIO (evoStep mRates currentGenoms ((<$> fitnesses) (+ (-fitnessPressure))))

  evoLoop startGenoms
  
  where gameFromGenoms appleCount startLength boardSize strength = createGame appleCount startLength boardSize . map (phenotype strength)


foo = (argMax (maximum . snd)) (zip games gameResults)
 where
   games = undefined :: [Int]
   gameResults = undefined :: [[Float]]


{- MENDING -}   

dnnElCrossover :: (DNN, DNN) -> Contingent DNN
dnnElCrossover (DNN f a, DNN _ b) = DNN f <$> mapM (mapM pickFromPair) (zipWith zipMatrices a b)

dnnMeanCrossover :: (DNN, DNN) -> Contingent DNN
dnnMeanCrossover (DNN f a, DNN _ b) = DNN f <$> bernoulliPick 
                                        0.9 (a, map (fmap pairMean) 
                                                (zipWith zipMatrices a b)) 



