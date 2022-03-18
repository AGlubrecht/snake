module CA where

--Improvements:
--Remember the actual environmentin each convolution-step
--Dont create a new grid-array in every CA-step


import qualified Data.Vector as Vec

import Data.List           ( maximumBy )
import Data.Array          ( (!), array )
import Control.Monad       ( forM_ )
import Control.Monad.State ( forM_, evalState, MonadState(get, put) )

import Types               ( CellState, Board, Position((:|:)), Action, actions )
import Util                ( argMax )
import Lattice             ( neumannNeighborhood, relDir, spirals )
import NeuralNet           ( DNN, Quantifyable(dimension, embed), runFloat )


cdim :: Int
cdim = max 4 (dimension (undefined :: CellState))

embedIn :: Quantifyable a => Int -> a -> Vec.Vector Float
embedIn dim q = embed q Vec.++ Vec.replicate (dim - dimension q) 0 
              --embed q in a a vector of size dim by adding zeros at the end

runCA :: Int -> DNN -> Board -> Action
runCA range dnn env = evalState steps 
  (array ((-range :|: -range), (range :|: range)) --The array only ever gets partly initialized
         [(pos, embedIn cdim (env pos)) | pos <- spirals!!range])

  where
    concentricSpirals = (reverse . tail . take range) spirals

    steps = do
      forM_ concentricSpirals (\spiral -> do --Update a smaller area with each iteration

        prev <- get
        let curr = array (-range :|: -range, range :|: range) --The array should become smaller with each iteration
                         [(pos, updateCell prev pos) | pos <- spiral] 
        put curr)
        
      field <- get
      return $ argMax (Vec.last . (field!) . relDir) actions

    updateCell prev pos = runFloat dnn 
      (Vec.generate cdim (\i -> sum (map (Vec.!i) neighbors))
       Vec.++ prev!pos)
       --the dnn input consists of the sum of the surrounding nodes' activations, 
       --concatenated with the cells own activations

      where neighbors = map (prev!) (neumannNeighborhood pos)
