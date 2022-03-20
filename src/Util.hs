module Util where

import Data.List ( findIndices, maximumBy )


{- ARITHMETIC -}

infty :: Float
infty = 10000000000

fat1 :: Num p => Bool -> p
fat1 True  = 1
fat1 False = 0

argMax :: Ord b => (a -> b) -> [a] -> a
argMax = maximumBy . (compare `on`)


mean :: (Fractional a, Foldable t) => t a -> a
mean nums = sum nums / fromIntegral (length nums)

pairMean :: (Float, Float) -> Float
pairMean = (/2) . uncurry (+)


{- COMBINATORS -}

infix 8 ..< 
(..<) :: (r -> z) -> (x -> y -> r) -> (x -> y -> z)
f ..< g = curry (f . uncurry g)

on :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
on g f a b = g (f a) (f b)


{- TYPE-GLUE -}

mapPair :: (a->b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

len :: [a] -> Float
len = fromIntegral . length


{- DEBUGGING -}

assert :: Bool -> a -> a
assert False = error "wrong assertion"
assert True  = id

asserting :: a -> [Bool] -> a
asserting a bs = case findIndices not bs of
  [] -> a
  wrongs -> error $ "The following assertions were violated: " ++ show wrongs





