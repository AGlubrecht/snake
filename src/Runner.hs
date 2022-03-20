module Runner where

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







r1 :: IO ()
r1 = runEvolution
    (return $ zeroDNN reLU [dimension (surrounding 3 (const Clear)), 3, dimension L])
    (evoStep . mutate)
    (const $ fromPureP . phaseify . perceiver (surrounding 3))
    (createGame)
    (scoreGame)
    (`gameToWidget` screenRadius)
    dnnInfo
    7
    40



runEvolution :: 
     Contingent genom -> ([Float] -> [genom] -> [Float] -> Contingent [genom])  
  -> (Int -> genom -> phenotype) -> (GameSettings -> [phenotype] -> Contingent game) -> (game -> [Float])
  -> (MVar game -> Widget game) -> ([Float] -> [genom] -> String)
  -> Int -> Int -> IO ()

runEvolution
  randomGenom evoStep 
  phenotype createGame scoreGame 
  gameToWidget info
  core_count games_per_core = do

  genCounter <- newIORef 0
  cLift <- cLifter ---unneccessary

  configVar <- newMVar defaultConfigs --actual start configurationt
  batchSize <- (core_count * games_per_core *) . snakesPerGame <$> readMVar configVar 
  startGenoms <- toIO $ replicateM batchSize randomGenom
  
  gameVar <- newMVar =<< toIO (gameFromGenoms (GameSettings (IterationSettings 0 False 100) (StartSettings 1 3 4)) 2 startGenoms)
  scoreLog <- new
  --ageLog <- new
  guiFromWidget (translate 300 0 (gameToWidget gameVar) 
            <:> avgWidget 10 scoreLog (-700, -0) (300, 300) 
            -- <:> plotToWidget ageLog (-700, -600) (300, 300)
            <:> confWidget configVar)
  print 0
  let evoLoop currentGenoms = do
        (Configs 
          strength 
          mRates  
          snakes_per_game  
          gameSettings) <- readMVar configVar


        let batchSize = core_count * games_per_core * snakes_per_game
        currGenCount <- readIORef genCounter
        games <- (toIO . mapM (gameFromGenoms gameSettings strength). chunksOf snakes_per_game) currentGenoms
        let gameResults = scoreGame <$> games
                        `using` parListChunk games_per_core rdeepseq

        swapMVar gameVar ((fst . argMax (maximum . snd)) (zip games gameResults))
        
        let fitnesses = concat gameResults
        putStrLn $ "Generation: " ++  show currGenCount
              ++ info fitnesses currentGenoms

        --ageLog += [mean ages]
        scoreLog += [mean fitnesses]

        writeIORef genCounter (currGenCount+1)
        evoLoop =<< toIO (evoStep mRates currentGenoms fitnesses)

  evoLoop startGenoms
  
  where gameFromGenoms gameSettings strength = createGame gameSettings . map (phenotype strength)



{- MENDING -}   

dnnElCrossover :: (DNN, DNN) -> Contingent DNN
dnnElCrossover (DNN f a, DNN _ b) = DNN f <$> mapM (mapM pickFromPair) (zipWith zipMatrices a b)

dnnMeanCrossover :: (DNN, DNN) -> Contingent DNN
dnnMeanCrossover (DNN f a, DNN _ b) = DNN f <$> bernoulliPick 
                                        0.9 (a, map (fmap pairMean) 
                                                (zipWith zipMatrices a b)) 



