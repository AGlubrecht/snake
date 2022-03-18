module Runner where

--high-score: b4: 16
--            b3: 11


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





r1 :: IO ()
r1 = runEvolution
    (return $ zeroDNN reLU [dimension (surrounding 3 (const Clear)), {-6,-} dimension L])
    (evoStep{-2 dnnMeanCrossover-} . mutate)
    (const $ fromPureP . strictPhaseify . perceiver (surrounding 3))
    (createGame 2 1)
    (\gameLength -> map (fromIntegral . score) . players . flip (!!) gameLength . iterate gameStep)
    (`gameToWidget` screenRadius)
    dnnVariance
    7
    40

r2 :: IO ()
r2 = runEvolution
    ({-randomDNN-}return $ zeroDNN reLU [cdim*2{-}, cdim + cdim `div` 2-}, cdim]) --initGenom
    (evoStep . mutate) --evoStep
    ((fromPureP . strictPhaseify) ..< runCA) --phenotype
    (createGame 2 1) ---
    (\gameLength -> map (fromIntegral . score) . players . flip (!!) gameLength . iterate gameStep)
    (`gameToWidget` screenRadius)
    {-last --(mean . map abs . concatMap unshape)--}dnnVariance
    7  --core_count 
    40--15 --games_per_core



runEvolution :: 
  Show info =>
     Contingent genom -> ([Float] -> [genom] -> [Float] -> Contingent [genom])  
  -> (Int -> genom -> phenotype) -> (Int -> [phenotype] -> Contingent game) -> (Int -> game -> [Float])
  -> (MVar game -> Widget game) -> ([genom] -> info)
  -> Int -> Int -> IO ()

runEvolution
  randomGenom evoStep 
  phenotype createGame scoreGame 
  gameToWidget variance
  core_count games_per_core = do

  genCounter <- newIORef 0
  cLift <- cLifter ---unneccessary

  configs <- newMVar (Configs 3 [0.5, 1, 1]  100 1 4 0) --actual start configurationt
  batchSize <- (core_count * games_per_core *) . snakesPerGame <$> readMVar configs 
  startGenoms <- toIO $ replicateM batchSize randomGenom
  
  gameVar <- newMVar =<< toIO (gameFromGenoms 4 2 startGenoms)
  scoreLog <- new
  --ageLog <- new
  guiFromWidget (gameToWidget gameVar 
            <:> avgWidget 10 scoreLog (-700, -0) (300, 300) 
            -- <:> plotToWidget ageLog (-700, -600) (300, 300)
            <:> confWidget configs)
  print 0
  let evoLoop currentGenoms = do
        (Configs strength mRates gameLength snakes_per_game boardSize fitnessPressure) <- readMVar configs
        let batchSize = core_count * games_per_core * snakes_per_game
        currGenCount <- readIORef genCounter
        --print 1
        games <- (toIO . mapM (gameFromGenoms boardSize strength). chunksOf snakes_per_game) currentGenoms
        --print 2
        swapMVar gameVar (head games)
        let gameResults = scoreGame gameLength <$> games
                        `using` parListChunk games_per_core rdeepseq
        --print gameLength
        let fitnesses = concat gameResults
        --print fitnesses
        putStrLn $ "Generation: "  ++  show             currGenCount
              ++ " Max fitness: "  ++ (show . maximum)  fitnesses
              ++ " Mean fitness: " ++ (show . mean)     fitnesses
              ++ " Variance: "     ++ (show . variance) currentGenoms
              ++ " Snakes: "       ++ (show . length)   fitnesses
        --ageLog += [mean ages]
        scoreLog += [mean fitnesses]

        writeIORef genCounter (currGenCount+1)
        evoLoop =<< toIO (evoStep mRates currentGenoms ((<$> fitnesses) (+ (-fitnessPressure))))

  evoLoop startGenoms
  
  where gameFromGenoms boardSize strength = createGame boardSize . map (phenotype strength)


{- MENDING -}   

dnnElCrossover :: (DNN, DNN) -> Contingent DNN
dnnElCrossover (DNN f a, DNN _ b) = DNN f <$> mapM (mapM pickFromPair) (zipWith zipMatrices a b)

dnnMeanCrossover :: (DNN, DNN) -> Contingent DNN
dnnMeanCrossover (DNN f a, DNN _ b) = DNN f <$> bernoulliPick 
                                        0.9 (a, map (fmap pairMean) 
                                                (zipWith zipMatrices a b)) 


dnnVariance :: [DNN] -> Float
dnnVariance = variance . map unshape




