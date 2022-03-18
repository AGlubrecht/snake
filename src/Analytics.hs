module Analytics where

import Util      ( (..<), mean )
import Data.List ( transpose )

--statistics

meanList :: [[Float]] -> [Float]
meanList = map mean <$> transpose

variance :: [[Float]] -> Float
variance vecs = mean $ (**(1/2)) . sum . zipWith ((**2) ..< (-)) avgVec <$> vecs
  where avgVec = meanList vecs
  

--pca is the eigendecomposition of covariance-matrix