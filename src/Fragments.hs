module Fragments where

{-monadicFinder :: PurePolicy
monadicFinder env = safeAction env (maybe (avoider env) goTowards nextPathPos)
  where 
    nextPathPos = do --itsa monad
      pos <- findClosest Appel env
      safeHead (getPath env (0, 0) pos)-}

{-


{-getPath :: Board -> Position -> Position -> [Position]
getPath board initialPosition targetPosition = evalState
  (latticeBFS initialPosition targetPosition)
  (SearchState2 
    (Set.singleton initialPosition) 
    (array (boundsOf searchRange) [(x:|:y, Nothing) | x <- [-searchRange.. searchRange-1],
                                                y <- [-searchRange.. searchRange-1]]) --why is emppty array so hard to make?
    (toArr searchRange board) 
    0
  )
--toArr
latticeBFS :: Position -> Position -> State SearchState2 [Position]
latticeBFS initialPos targetPos = do
  SearchState2 searchSet parentOf arrBoard pathLength <- get
  if Set.null searchSet
    then return []
    else do
      if parentOf!targetPos == Nothing 
      then do
        let isFree pos = ttl (fromArr arrBoard pos) < pathLength

            parents = Set.elems searchSet

            childrens = map (filter isFree . neumannNeighborhood) parents

            searchSet' = (Set.fromList . concat) childrens

            parentOf' = parentOf // concat [zip children (repeat $ Just parent) 
                    | (children, parent) <- zip childrens parents]
            
            arrBoard' = arrBoard // zip parents (repeat Wall) --should be Snek ? (score+pathlength) Wall, but sneks don't know their own score
            
            pathLength' = pathLength+1

        put (SearchState2 searchSet' parentOf' arrBoard' pathLength')
        latticeBFS initialPos targetPos
      else do
        let pathFrom pos = case parentOf!pos of
              Nothing       -> []
              Just prevPos -> pos:pathFrom prevPos 
        return (reverse $ pathFrom targetPos)-}

{-getShortestPath :: Board -> Position -> Position -> [Position]
getShortestPath board initialPosition targetPosition = evalState
  (bfs board initialPosition targetPosition)                                           --operation on state
  (BFSState (Seq.singleton initialPosition) (Set.singleton initialPosition) Map.empty) --starting sate

data BFSState = BFSState
  { bfsSearchQueue :: Seq.Seq Position
  , bfsVisistedPositions :: Set.Set Position
  , bfsParents :: Map.Map Position Position
  }

bfs :: Board -> Position -> Position -> State BFSState [Position]
bfs board initialPosition targetPosition = do
  BFSState searchQueue visitedSet parentsMap <- get
  if Seq.null searchQueue
    then return [] --no path was found
    else do
      let nextLoc = Seq.index searchQueue 0
      if nextLoc == targetPosition  --path found :)
        then return (unwindPath parentsMap [targetPosition])
        else do  --do a step of bfs
          let adjacentCells = getAdjacentPositions board nextLoc
              unvisitedNextCells = filter (\l -> not (Set.member l visitedSet)) adjacentCells
              newSearchQueue = foldr (flip (Seq.|>)) (Seq.drop 1 searchQueue) unvisitedNextCells --mabe modify bfs here and remove set
              newVisitedSet = Set.insert nextLoc visitedSet
              newParentsMap = foldr (\l -> Map.insert l nextLoc) parentsMap unvisitedNextCells
          put (BFSState newSearchQueue newVisitedSet newParentsMap)
          bfs board initialPosition targetPosition
  where
    unwindPath parentsMap currentPath = case Map.lookup (head currentPath) parentsMap of
      Nothing -> tail currentPath --reached beginning of path
      Just parent -> unwindPath parentsMap (parent : currentPath)

getAdjacentPositions :: Board -> Position -> [Position]
getAdjacentPositions board position = filter (isFree . board) (neumannNeighbors position)-}


randomer :: Policy
randomer board = maybe arbitrary (actionToDist . goTowards) (findClosest Appel board)

arbitrary :: Action -> Float
arbitrary _ = 1

safeRandAction :: Board -> Distribution -> Distribution
safeRandAction board distri action | board (relDir action) == Clear = distri action
                                   | otherwise                      = 0

safeAction :: Board -> Action -> Action
safeAction env action | env (relDir action) == Clear = action
                      | otherwise                    = reasonable env

harvester :: PurePolicy
harvester env = safeAction env (maybe F goTowards (findClosest Appel env))-}

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

{-

livingPlayers :: Game -> [Player]
livingPlayers = filter ((Alive ==). status) . players

getScore :: Player -> Float
getScore (Player _ score _ _ _ _ age) = fromIntegral (score - 4) + (fromIntegral age /20)

count :: (Foldable f, Eq a) => a -> f a -> Int
count value = length . filter (==value) . toList

getSnake :: Board -> Position -> [Position]
getSnake env = fromMaybe [] . getTail env

getTail :: Board -> Position -> Maybe [Position] --what is this why so complicated
getTail env pos = case env pos of

  SnakeHead _id n -> Just $ pos:fromMaybe [] (getTail env =<< preceedingTailPart pos)
    where
      preceedsCurr (Snek _id' m) 
        | _id' == _id && (m>=n-2) = True
      preceedsCurr _              = False
      preceedingTailPart pos      = find (preceedsCurr.env) (neumannNeighbors pos)

  Snek _id n -> Just $ pos:fromMaybe [] (getTail env =<< preceedingTailPart pos)
    where
      preceedsCurr (Snek _id' m) 
        | _id' == _id && (m==n-1 || m==n-2) = True
      preceedsCurr _                        = False
      preceedingTailPart pos                = find (preceedsCurr.env) (neumannNeighbors pos)

  _            -> Nothing-}

{-change :: Array Int Int -> ST s (ST s (Array  Int Int))
change arr = do
  --let arr = arrBoard game
  stArr <- thaw arr :: ST s (STArray s Int Int)
  writeArray stArr 2 3
  return $ freeze stArr-}

{-fastStep :: STArray s Position CellState -> Game -> ST Game
fastStep stBoard g =
  writeArray stBoard-}

{-class Mating a where
    (>|<) :: a -> a -> Contingent a-}

{-instance Mating Float where
  x >|< y = return $ (x + y)/2-}

{-instance Mating [a] where
  tx >|< ty =  mapM pickFromPair (zip tx ty)
    where pickFromPair (a, b) = do
            randBool <- getRandom
            return (if randBool then a else b)-}


{-strat :: Int -> Strategy [[Float]]
strat gamesPerIt = parListChunk parChunkSize rdeepseq
  where parChunkSize = ceiling $ fromIntegral gamesPerIt / fromIntegral parallelWorkers-}

--{-# LANGUAGE TypeFamilies #-}

--{-# LANGUAGE TypeOperators #-}

{-module Game where


import Data.Array
import Data.STRef
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.State
import Control.Monad.ST
import System.IO
import Data.Char
import Control.Monad (when)

readwrite :: IO ()
readwrite = do
    contents <- readFile "/file.txt"
    let newContents = map toUpper contents
    when (length newContents > 0) $
        writeFile "/file.txt" newContents


class Sty t where
  type Property t
  getSubtype :: t -> Property t

data DATA = DATA {
  _1 :: Int, 
  _2 :: Int
} deriving (Show, Read)

data1 :: DATA
data1 = DATA 2 7-}
{-data Sty t => Subtyped t = Subtyped {
  basic :: t,
  property :: Property t
}

data Attempt a = Success a | Failure String
instance Show (Attempt a) where
  show (Success _) = "Success"
  show (Failure s) = "Failure: " ++ s

{-instance Functor Attempt where
  fmap f (Failure s) = Failure s
  fmap f (Success a) = Success (f a)
instance Applicative Attempt where
  pure = Success
  (Failure s) <*> _ = Failure s
  _ <*> (Failure s) = Failure s
  (Success f) <*> (Success a) = Success (f a)
instance Monad Attempt where
  f >>= g = -}


instance Sty [a] where
  type Property [a] = Int 
  getSubtype as = length as

data (|->|) a b = SFuncIntermediary a b :|* String
data  SFuncIntermediary a b = (a->b) ::| (Property a -> Attempt (Property b))
infix 2 ::|
infix 1 :|*

(|<<) :: (Sty a,  Sty b) => a |->| b -> a -> b
(|<<) (basicF ::| typeF :|* name) = basicF 
(|<*) :: (Sty a,  Sty b) =>  a |->| b -> Property a -> Attempt (Property b)
(|<*) (basicF ::| typeF :|* name) = typeF
(|<<|) :: (Sty a,  Sty b) =>  a |->| b -> a -> b 
(|<<|) f a = case f |<* getSubtype a of
  Failure s -> (error . show . Failure) s
  Success b -> f |<< a

instance (Sty a,  Sty b) => Show (a |->| b) where
  show (basicF ::| typeF :|* name) = name

(<-<) :: (Sty a,  Sty b, Sty c) => b |->| c -> a |->| b -> a |->| c
g <-< f = (g |<<) .   (f |<<) 
      ::| (\a -> case f |<* a of
        Success b -> g |<* b
        Failure s -> Failure $ show g ++ " <- " ++ s)

      :|* (show g ++ " <-< " ++ show f)

sTail :: [a] |->| [a]
sTail = tail 
    ::| (\l -> if l > 0 then Success (l-1) else Failure "(tail [])")
    :|* "tail"

--quicksort2 :: (Ord a) => Array Int a -> Array Int a
--quicksort2 :: Ord a => Array Int a -> ST s (STArray s Int a)
{-quicksort2 inputArr = do
  stArr <- thaw inputArr
  let (minIndex, maxIndex) = bounds inputArr
  quicksort2Helper minIndex (maxIndex + 1) stArr
  return stArr

quicksort2Helper :: (Ord a)
  => Int 
  -> Int
  -> STArray s Int a
  -> ST s ()
quicksort2Helper start end stArr = when (start + 1 < end) $ do
  pivotIndex <- partition stArr start end
  quicksort2Helper start pivotIndex stArr
  quicksort2Helper (pivotIndex + 1) end stArr    

partition :: (Ord a)
 => STArray s Int a
 -> Int
 -> Int
 -> ST s Int
partition arr start end = do
  pivotElement <- readArray arr start
  let pivotIndex_0 = start + 1
  finalPivotIndex <- execStateT
    (mapM (partitionLoop arr pivotElement) [(start+1)..(end-1)])
    pivotIndex_0
  swap arr start (finalPivotIndex - 1)
  return $ finalPivotIndex - 1

partitionLoop :: (Ord a)
  => STArray s Int a
  -> a
  -> Int
  -> StateT Int (ST s) ()
partitionLoop arr pivotElement i = do
  pivotIndex <- get
  thisElement <- lift $ readArray arr i
  when (thisElement <= pivotElement) $ do
    lift $ swap arr i pivotIndex
    put (pivotIndex + 1)


swap :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swap arr i j = do
  elem1 <- readArray arr i
  elem2 <- readArray arr j
  writeArray arr i elem2
  writeArray arr j elem1-}-}