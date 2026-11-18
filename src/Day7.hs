{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Day7(day7) where

import Utils 
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as I

--id=1, energy=5, connections=2,5,7
parse :: String -> (Int, (Int, [Int]))
parse s = (read $ init $ drop 3 (ws!!0), 
          (read $ init $ drop 7 $ ws!!1
          , read <$> words ((\c -> if c==',' then ' ' else c) <$> drop 12 (ws!!2))))
  where
    ws = words s


groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)

addEnergy :: Int -> M.Map Int (Int, [Int]) -> Int -> M.Map Int Int
addEnergy startEnergy startMap startId = go [(startId, 1)] startEnergy (M.map fst startMap)
  where
    go :: [(Int, Int)] -> Int -> M.Map Int Int -> M.Map Int Int
    go [] _ mp = mp
    go _ 0 mp = mp
    go ids energy mp = go nextIds newEnergy newMap
      where
        -- trace (show energy ++ " " ++ show newIds) $ 
        nextIds :: [(Int, Int)]
        --nextIds = (\g -> (fst $ head g, sum $ snd <$> g)) <$> groupOn fst (concat ((\(i,n) -> (,n) <$> (snd (startMap M.! i))) <$> ids))
        nextIds = M.toList $ M.fromListWith (+) [(j, n) | (i, n) <- ids, j <- snd (startMap M.! i)]
        newEnergy = energy - 1 
        newMap = foldl (\m (i, n) -> M.insertWith (+) i (n * energy) m) mp ids


maximumEnergy :: Int
maximumEnergy = 1000000

-- energy -> node id -> number of impulses at that node with that energy
type Impulses = I.IntMap (I.IntMap Int)


addEnergyWithExplosions :: Int -> M.Map Int (Int, [Int]) -> Int -> M.Map Int Int
addEnergyWithExplosions startEnergy startMap startId =
  M.map fst $ go (I.singleton startEnergy (I.singleton startId 1)) startMap
  where
    go :: Impulses -> M.Map Int (Int, [Int]) -> M.Map Int (Int, [Int])
    go impulses mp
     | I.null impulses = mp
     | otherwise = go (I.unionWith (I.unionWith (+)) nextImpulses newImpulses) (foldr M.delete newMap exploders)
      where
        addHit e m i n = M.adjust (\(en, cs) -> (en + n * e, cs)) i m
        newMap = I.foldlWithKey (\m e cs -> I.foldlWithKey (addHit e) m cs) mp impulses

        exploders = M.keys $ M.filter ((>= maximumEnergy) . fst) newMap
        conns i = maybe [] snd (M.lookup i newMap)
        -- For each exploder, lookup the connections and turn them into impulses
        newImpulses
          | null exploders = I.empty
          | otherwise      = I.singleton startEnergy $ I.fromListWith (+) [(j, 1) | i <- exploders, j <- conns i]

        step :: I.IntMap Int -> I.IntMap Int
        step cs = I.fromListWith (+)
                  [(k, n) | (j, n) <- I.toList cs, j `M.member` newMap, k <- snd (startMap M.! j)]
        nextImpulses = I.mapKeysMonotonic (subtract 1) $ I.map step $ I.filterWithKey (\e _ -> e > 1) impulses


day7 :: IO ()
day7 = do
  s1 <- getLines (7, 1)
  s2 <- getLines (7, 2)
  s3 <- getLines (7, 3)
  --let s1 = test
  let mp1 = M.fromList $ parse <$> s1
      mp2 = M.fromList $ parse <$> s2
      mp3 = M.fromList $ parse <$> s3

      start1 :: (Int, (Int, [Int]))
      start1 = head $ sortOn ((*(-1)) . length . snd . snd) $ M.toList mp1
      startId1 = fst start1

      start2 :: (Int, (Int, [Int]))
      start2 = head $ sortOn ((*(-1)) . length . snd . snd) $ M.toList mp2
      startId2 = fst start2

      finish1 = addEnergy 5 mp1 startId1
      finish2 = addEnergy 10 mp2 startId2
      finish3 = addEnergyWithExplosions 10 mp3 <$> M.keys mp3


  putStrLn $ "Quest7: part1: " ++ show (sum $ M.elems finish1)
  putStrLn $ "Quest7: part2: " ++ show (sum $ M.elems finish2)
  putStrLn $ "Quest7: part3: " ++ show (maximum $ sum . M.elems <$> finish3)
  return ()


test = ["id=1, energy=5, connections=2,5,7"
  , "id=2, energy=9, connections=6,7"
  , "id=3, energy=3, connections=4"
  , "id=4, energy=8, connections=3"
  , "id=5, energy=1, connections=6,7"
  , "id=6, energy=7, connections=1,7"
  , "id=7, energy=6, connections=5"]

