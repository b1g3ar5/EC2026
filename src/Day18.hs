{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Day18(day18) where

import Utils hiding (rt, lt)
import qualified Data.Map as M


parse :: [String] -> M.Map Int Int
parse ls = M.fromList cs
  where
    cs = go <$> ls
    go l = (read $ drop 2 $ ws!!0, read $ drop 2 $ ws!!1)
      where
        ws = words l


parse3 :: [String] -> M.Map Int (Int, Int)
parse3 ls = M.fromList cs
  where
    cs = go <$> ls
    go l = (read $ drop 2 $ ws!!0, (read $ drop 2 $ ws!!1, read $ drop 6 $ ws!!2))
      where
        ws = words l


-- Map lookup with default 0
lu :: M.Map Int Int -> Int -> Int
lu mp x = fromMaybe 0 $ mp M.!? x


hmax :: Int -> Int -> Int -> Int
hmax lt b rt
  | b >= lt = b
  | b >= rt = b
  | lt > rt = rt
  | otherwise = lt


makeKey :: M.Map Int Int -> [Coord]
makeKey mp = height 0 [0..(maximum $ M.keys mp)]
  where
    height :: Int -> [Int] -> [Coord]
    height _ [] = error "Too far"
    height _ [x] = [(x, mp `lu` x)]
    height lt (x:xs) = (x, hmax lt (mp `lu` x) rt) : height (max lt $ mp `lu` x) xs
      where
        rt = maximum $ (mp `lu`) <$> xs


-- This looks neat now ...
makeKey3 :: M.Map Int (Int, Int) -> Int
makeKey3 mp = area stats
  where
    hs = map fst (M.elems mp)  
    
    -- The input with max height to left and right including the current height.
    -- Took me a while to work out this was what scanl is for.
    stats :: [((Int, (Int, Int)), Int, Int)]
    stats = zip3 (M.toList mp) (scanl1 max hs) (scanr1 max hs)
    
    area [] = 0
    area [((_, (_, w)), lt, rt)] = min lt rt * w
    -- rt1 for the w columns, rt2 for the gap columns
    area (((x1, (_, w1)), lt1, rt1) : rest@(((x2, _), _, rt2) : _)) =
      min lt1 rt1 * w1 + min lt1 rt2 * (x2 - x1 - w1) + area rest


day18 :: IO ()
day18 = do
  s1 <- getLines (18, 1)
  s2 <- getLines (18, 2)
  s3 <- getLines (18, 3)
  let cs1 = parse s1
      cs2 = parse3 s2
      cs3 = parse3 s3

  putStrLn $ "Quest18: part1: " ++ show (sum $ snd <$> makeKey cs1)
  putStrLn $ "Quest18: part2: " ++ show (makeKey3 cs2)
  putStrLn $ "Quest18: part3: " ++ show (makeKey3 cs3)
  return ()

