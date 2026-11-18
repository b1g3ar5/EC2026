{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Day9(day9) where

import Utils hiding (Tree, Tree') 
import qualified Data.Vector as V
import Data.Vector (Vector) 
import Data.List (mapAccumL)
import qualified Data.Map as M

--id=1, energy=5, connections=2,5,7
parse :: String -> (Int, Int, Int)
parse s = ( read $ init $ drop 3 $ ws!!0 
          , read $ init $ drop 7 $ ws!!1
          , read $ drop 6 $ ws!!2
          )
  where
    ws = words s


type NodeId = Int

data Branch = Branch { from :: NodeId
                     , to :: NodeId
                     , len :: Int
                     } deriving (Show, Eq)

data Tree = Tree { ns :: Int
                 , bs :: Vector Branch
                 , growth :: Int
                 , limit :: Int
                 } deriving (Show, Eq)                 

type SplitTime = Int

data Tree' = Tree' { ns' :: Int
                   , bs' :: M.Map SplitTime Int
                   , growth' :: Int
                   , limit' :: Int
                   } deriving (Show, Eq)                 

size :: Tree -> Int
size t = sum (V.map len (bs t)) + (ns t - 2) `div` 2

leaves :: Tree' -> Int
leaves t = ns' t `div` 2 

makeTree :: (Tree' -> Int) -> (Int, Int) -> Int
makeTree limitFn' (g, l) = go 1 $ Tree' { ns' = 2, bs' = startBranches, growth' = g, limit' = l }
  where
    -- The first branch has length 1 at second 1, so it splits at second g
    startBranches = M.singleton g 1

    go :: Int -> Tree' -> Int
    go s t
      | limitFn' t >= limit' t = s
      | otherwise = 
          case M.minViewWithKey (bs' t) of
            Just ((splitTime, count), rest) -> go splitTime $ splitBatch splitTime count t { bs' = rest }
            Nothing -> s --finished


-- Split c branches at second s. A branch of length n at second s splits at s + growth - n
splitBatch :: SplitTime -> Int -> Tree' -> Tree'
splitBatch s c t = t { ns' = ns' t + 2 * c
                     , bs' = M.insertWith (+) (s + g - half) (2 * c)
                           $ M.insertWith (+) (s + g - 1) c
                           $ bs' t }
  where
    g = growth' t
    half = g `div` 2


day9 :: IO ()
day9 = do
  s1 <- getLines (9, 1)
  s2 <- getLines (9, 2)
  s3 <- getLines (9, 3)
  let --s1 = test
      ls1 = makeTree' size . (\(_,a,b) -> (a,b)) . parse <$> s1
      ls2 = makeTree leaves . (\(_,a,b) -> (a,b)) . parse <$> s2
      ls3 = makeTree leaves . (\(_,a,b) -> (a,b)) . parse <$> s3

  putStrLn $ "Quest9: part1: " ++ show (sum ls1)
  putStrLn $ "Quest9: part2: " ++ show (sum ls2)
  putStrLn $ "Quest9: part3: " ++ show (sum ls3)
  --putStrLn $ "Quest9: part2: " ++ show (sum $ M.elems finish2)
  --putStrLn $ "Quest9: part3: " ++ show (maximum $ sum . M.elems <$> finish3)
  return ()


-- OLD INITIAL PART2 CODE

leaves' :: Tree -> Int
leaves' t = ns t `div` 2 

makeTree' :: (Tree -> Int) -> (Int, Int) -> Int
makeTree' limitFn (g, l) = go 1 $ Tree { ns = 2, bs = startBranches, growth = g, limit = l }
  where
    startBranches = V.singleton $ Branch 0 1 1

    go :: Int -> Tree -> Int
    go s t
      | limitFn t >= limit t = s 
      | otherwise = go (s+1) $ addNodes' t
      where 
        msg = show (sum $ V.map len (bs t)) ++ ", " ++ show (limit t)
               

addNodes' :: Tree -> Tree
addNodes' t = t { ns = n', bs = V.fromList (concat bss) }
  where
    (n', bss) = mapAccumL step (ns t) (V.toList (bs t))
    half = growth t `div` 2
    step :: Int -> Branch -> (Int, [Branch])
    step n b
      | len b < growth t - 1 = (n, [b {len = len b + 1}])
      | otherwise = (n + 2, [ Branch (from b) n half
                            , Branch n (to b) half
                            , Branch n (n + 1) 1 ])


