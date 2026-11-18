{-# LANGUAGE FlexibleContexts #-}

module Day6(day6) where

import Utils 
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M


-- HNYN:BRDS,FLWR,SLTT,YSTS
parse :: String -> (String, Set String)
parse s = (ps!!0, S.fromList $ words $ (\c -> if c==',' then ' ' else c) <$> ps!!1)
  where
    ps = splitOn ':' s


allNecessary :: M.Map String (Set String) -> String -> Set String
allNecessary mp start = go (S.singleton start) (S.singleton start)
  where
    go :: Set String  -> Set String  -> Set String
    go pipeline seen
      | S.null pipeline = seen
      | S.null toAdd = go rest seen
      | otherwise = go (S.union rest toAdd) (S.union seen toAdd)
      where
        (next, rest) = fromJust $ S.minView pipeline
        toAdd = S.filter (`S.notMember` seen) $ fromMaybe (S.empty) $ M.lookup next mp


in2count :: [Set String] -> Set String
in2count [] = S.empty
in2count [ps] = S.empty
in2count (ps:pss) = qs `S.union` in2count pss
  where
    u = S.unions pss
    qs = S.filter (\p -> p `S.member` u) ps


day6 :: IO ()
day6 = do
  s1 <- getLines (6, 1)
  s2 <- getLines (6, 2)
  s3 <- getLines (6, 3)
  let ingredients1 = M.fromList $ parse <$> s1
      ingredients2 = M.fromList $ parse <$> s2
      -- Calculate the reverse map
      neededFor2 = M.fromListWith (S.union) [(v, S.singleton k) | k <- M.keys ingredients2, v <- S.toList $ ingredients2 M.! k]
      ingredients3 = M.fromList $ parse <$> s3
      neededFor3 = M.fromListWith (S.union) [(v, S.singleton k) | k <- M.keys ingredients3, v <- S.toList $ ingredients3 M.! k]
      
      allNull = allNecessary ingredients1 "NULL"
      allReverseNull = allNecessary neededFor2 "NULL"
      allReverseNulls = (allNecessary neededFor3) <$> ["NULLv1", "NULLv2", "NULLv3"]

  putStrLn $ "Quest6: part1: " ++ show (S.size allNull - 1) -- includes NULL, so subtract 1
  putStrLn $ "Quest6: part2: " ++ show (S.size allReverseNull - 1) -- includes NULL, so subtract 1
  putStrLn $ "Quest6: part3: " ++ show (S.size $ in2count allReverseNulls)
  return ()


