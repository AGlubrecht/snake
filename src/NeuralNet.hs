module NeuralNet where


import qualified Data.Vector as Vec

import Data.List     ( maximumBy, transpose )
import Data.Function ( on )
import Control.Monad ( (<=<) )
import Data.Matrix   ( Matrix, colVector, fromLists, getCol, multStd, toList, toLists, zero )

import Types         ( actions, Action, CellState(Appel, Wall, Snek, SnakeHead) )
import Util          ( (..<), fat1 )
import Random        ( Contingent )
import Evo           ( Evolving(..) )



class Quantifyable a where
  dimension :: a     -> Int
  embed    :: a     -> Layer
  extract   :: Layer -> a


data DNN = DNN {
  _activationFunc :: Float -> Float, 
  _weights        :: [Matrix Float]
}

type Layer = Vec.Vector Float



{- QUANTIFYABLE -}

instance Quantifyable CellState where
  dimension = const 4

  embed  x = Vec.fromList $ map fromIntegral (
    case x of
      Wall            -> [1, 0, 0, 1]
      (Snek _ n)      -> [0, 1, 0, 1]
      (SnakeHead _ n) -> [0, 1, 0, 1]
      Appel i         -> [0, 0, 1, 1]
      _               -> [0, 0, 0, 1])

  extract y = error "couldn't extract cellstate"

instance Quantifyable Action where
  dimension = const 3

  embed  x = Vec.fromList $ fat1 . (x==) <$> actions

  extract y = fst (maximumBy (compare `on` snd) (zip actions (Vec.toList y)))


instance Quantifyable q => Quantifyable (Vec.Vector q) where
  dimension = sum . fmap dimension

  embed    = (>>= embed)

  extract   = error "couldn't extract vector"



{- DNN -}

instance Show DNN where
  show (DNN _ weightMatrices) = "DNN: " ++ show weightMatrices

instance Evolving DNN where
  mutate rates (DNN f c) = DNN f <$> mutate rates c


run :: (Quantifyable a, Quantifyable b) => DNN -> a -> b
run dnn = extract . runFloat dnn . embed

runFloat :: DNN -> Layer -> Layer
runFloat (DNN activationFunc weightMatrices) = 
    getCol 1
  . (flip $ foldl (fmap activationFunc ..< flip multStd)) weightMatrices
  . colVector 


randomDNN :: (Float -> Float) -> [Int] -> Contingent DNN
randomDNN activationFunc layerSizes = mutate [1, 1, 1, 1] (zeroDNN activationFunc layerSizes)

zeroDNN :: (Float->Float) -> [Int] -> DNN
zeroDNN activationFunc layerSizes =
  DNN activationFunc
      (zipWith zero (tail layerSizes) (init layerSizes))


unshape :: DNN -> [Float]
unshape = toList <=< _weights

zipMatrices :: Matrix b -> Matrix b -> Matrix (b, b)
zipMatrices = fromLists . map (uncurry zip) ..< (zip `on` toLists)



{- ACTIVATION FUNCTIONS -}

sigmoid :: Float -> Float
sigmoid x = 1.0 / (1.0 + exp (-x))

reLU    :: Float -> Float
reLU    x = min x 0