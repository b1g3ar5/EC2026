{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Day11(day11) where

import Utils 
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.Array.Unboxed as U
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Control.Monad
import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import qualified Data.IntMap as M



type Ring = Array Int (Set Int)
type ShortCuts = Set (Int, Int)



makeRing1 :: Int -> ShortCuts -> Ring
makeRing1 n ss = A.array (0, (n-1)) $ elems
  where
    elems = [(i, (S.fromList $ [(i-1) `mod` n, (i+1) `mod` n]) `S.union` ks) | i <- [0..(n-1)], let ks = S.map snd $ S.filter ((==i) . fst) ss]

makeRing2 :: Int -> ShortCuts -> Ring
makeRing2 n ss = A.array (0, (n-1)) $ elems
  where
    elems = [(i, (S.fromList [(i-3) `mod` n,(i-2) `mod` n,(i-1) `mod` n, (i+1) `mod` n, (i+2) `mod` n, (i+3) `mod` n])
                 `S.union`  ks) | i <- [0..(n-1)], let ks = S.map snd $ S.filter ((==i) . fst) ss]


shortestPath :: Int -> Int -> Ring -> Int
shortestPath from to r = go (S.singleton (0,from)) (S.singleton from)
  where
    go :: Set (Int, Int) -> Set Int -> Int
    go pipeline seen
      | S.null pipeline = error "No pipeline"
      | p == to = n
      | to `S.member` nxt = (n+1)
      | otherwise = go (rem `S.union` ((n+1,) `S.map` nxt)) (nxt `S.union` seen )
      where
        Just ((n,p), rem) = S.minView pipeline
        nxt = S.filter (`S.notMember` seen) $ r A.! p

shortestPaths :: Ring -> Int -> Array Int Int
shortestPaths ring from = A.listArray (0, I.size mm - 1) (I.elems mm)
  where
    mm = go (S.singleton (0,from)) (I.singleton from 0)
    go :: Set (Int, Int) -> IntMap Int -> IntMap Int
    go pipeline mp
      | S.null pipeline = mp
      | otherwise = go (rem `S.union` ((n+1,) `S.map` nxt)) newMp
      where
        Just ((n,p), rem) = S.minView pipeline
        newMp = foldr (\k m -> I.insert k (n+1) m) mp (S.toList nxt)
        nxt = S.filter (`I.notMember` mp) $ ring A.! p


--resonance :: (Int -> ShortCuts -> Ring) -> Int -> ShortCuts -> Int
resonance mk p sc = 2 * sum t2
  where
    r1 = mk p S.empty
    r2 = mk p sc
    t2 :: [Int]
    t2 = do
      i <- [0..(p-1)]
      j <- [0..(i-1)]
      guard $ i /= j
      let d1 = shortestPath i j r1
      let d2 = shortestPath i j r2
      return $ d1 - d2


-- Rewritten using the fast part 3 logic
bestResonance2 :: Int -> ShortCuts -> Int
bestResonance2 p sc =  maximum rs
  where
    baseline = sum $ U.elems $ distanceMatrix p S.empty
    dm = distanceMatrix p sc
    rs :: [Int]
    rs = [ baseline - sum (U.elems $ addEdge p dm (c1, c2) ) | c1 <- [0..p-1], c2 <- [0..c1-1]]


distanceMatrix :: Int -> ShortCuts -> UArray (Int, Int) Int
distanceMatrix p sc = U.array ((0,0),(p-1,p-1)) $ concatMap (\(i,arr) -> (\(j,v) -> ((i,j), v)) <$> U.assocs arr) ds
  where 
    rg = makeRing2 p sc
    ds = (\i -> (i, shortestPaths rg i)) <$> [0..(p-1)]


min3 :: Ord a => [a] -> a
min3 [a,b,c] = min a $ min b c

addEdge :: Int -> UArray (Int,Int) Int -> (Int,Int) -> UArray (Int,Int) Int
addEdge n d (a,b) = U.listArray ((0,0),(n-1,n-1))
      [min3 [ d U.! (i,j)
        , d U.! (i,a) + 1 + d U.! (b,j)
        , d U.! (i,b) + 1 + d U.! (a,j)
      ] | i <- [0..n-1], j <- [0..n-1]]


-- pairings for the crystal paths
pairings :: [Int] -> [[(Int, Int)]]
pairings []     = [[]]
pairings (x:xs) = [ (x,y):rest | y <- xs, rest <- pairings (filter (/= y) xs) ]


largestResonance :: Int -> ShortCuts -> Int
largestResonance p sc = maximum chooseCrystals
  where
    baseline = sum $ U.elems $ distanceMatrix p S.empty
    dm = distanceMatrix p sc
    chooseCrystals = do
      let step = p `div` 8
      offset <- [0..step-1]
      let crystals = [offset, offset+step .. p-1]
      ps <- pairings crystals
      let d = foldl' (addEdge p) dm ps
      return $ baseline - sum (U.elems d)


day11 :: IO ()
day11 = do
  putStrLn $ "Quest11: part1: " ++ show ( resonance makeRing1 portals1 shortCuts1)
  putStrLn $ "Quest11: part2: " ++ show ( bestResonance2 portals2 shortCuts2)
  putStrLn $ "Quest11: part3: " ++ show (largestResonance portals3 shortCuts3)
  return ()


testPortals, portals1, portals2, portals3 :: Int
portals1 = 33
portals2 = 44
portals3 = 88
testPortals = 10


testShortCuts :: ShortCuts
testShortCuts = S.fromList [(4,9), (1,6), (9, 4), (6, 1)]


shortCuts2' :: ShortCuts
shortCuts2' = S.fromList [(12, 7), (6, 20), (41, 3), (16, 24), (37, 30)]
shortCuts3' = S.fromList [(56, 40), (5, 60), (12, 75), (69, 56)]

shortCuts1, shortCuts2, shortCuts3 :: ShortCuts
shortCuts1 = S.fromList [(20,28), (27,30), (28, 20), (30, 27)]
shortCuts2 = shortCuts2' `S.union` (S.map swap shortCuts2')
shortCuts3 = shortCuts3' `S.union` (S.map swap shortCuts3')


{-
portals: 88

57 41
6 61
13 76
70 57
-}