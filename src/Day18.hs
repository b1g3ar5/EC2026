{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Day18(day18) where

import Utils
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Map as M


day18 :: IO ()
day18 = do
  s1 <- getLines (18, 1)
  --s2 <- getLines (18, 2)
  --s3 <- getLines (18, 3)
  let grid1 = S.fromList $ fst <$> filter snd (parseGridWith (=='#') s1)
      --grid2 = fst <$> filter snd (parseGridWith (=='#') s2)
      --grid3 = S.fromList $ fst <$> filter snd (parseGridWith (=='#') s3)
      --sides = (\c -> 4 - length (filter (`elem` grid2) $ neighbours4 c)) <$> grid2

  putStrLn $ "Quest18: part1: " ++ show ("")
  --putStrLn $ "Quest18: part2: " ++ show (sum sides)
  --putStrLn $ "Quest18: part3: " ++ show (count3 grid3)
  return ()

