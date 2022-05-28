module Analytics where


import Numeric ( showFFloat )
import Util      ( (..<), mean, len )
import Data.List ( transpose, sort )
import NeuralNet


formatNum :: RealFloat a => Int -> a -> String
formatNum length num = showFFloat (Just length)  num ""

{- NEURAL NETWORKS -}

dnnInfo :: [Float] -> [DNN] -> String 
dnnInfo fitnesses dnns = 
               " | Max Fitness: "    ++ (formatNum 0 . maximum                ) fitnesses
            ++ " | Mean Fitness: "   ++ (formatNum 2 . mean                   ) fitnesses
            ++ " | Median Fitness: " ++ (formatNum 1 . median                 ) fitnesses
            ++ " | Max Rate: "       ++ (formatNum 3 . maxRate                ) fitnesses
            ++ " | Variance: "       ++ (formatNum 3 . dnnVariance            ) dnns
            ++ " | Mean Weight: "    ++ (formatNum 3 . mean . (meanWeight <$>)) dnns

dnnVariance :: [DNN] -> Float
dnnVariance = variance . map unshape

meanWeight :: DNN -> Float
meanWeight = mean . map abs . unshape


{- GENERAL -}

meanList :: [[Float]] -> [Float]
meanList = map mean <$> transpose

variance :: [[Float]] -> Float
variance vecs = mean $ (**(1/2)) . sum . zipWith ((**2) ..< (-)) avgVec <$> vecs
  where avgVec = meanList vecs

median :: [Float] -> Float 
median nums | null nums = 0
            | otherwise  = (upper + lower) / 2
  where
    sorted = sort nums
    half   = length sorted `div` 2
    lower  = sorted !! half
    upper  = reverse sorted !! half

rate :: Eq a => a -> [a] -> Float 
rate a as = len (filter (a==) as) / len as 

maxRate :: [Float] -> Float 
maxRate nums = rate (maximum nums) nums

  

--pca is eigendecomposition of covariance-matrix