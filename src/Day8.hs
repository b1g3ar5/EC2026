{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Day8(day8) where

import Utils 
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as U


type Grid = UArray Coord Int

defaultValue:: Int
defaultValue = -99

parse' :: [String] -> M.Map Coord Int
parse' ss = M.fromList $ [ ((x,y), c')| (y,l) <- zip [0..] ss, (x,c) <- zip [0..] l, let c' = if c `elem` ['0'..'9'] then read [c] else 0, c' /= 0]

parse :: [String] -> Grid
parse ss = U.accumArray (\_ c -> c) defaultValue ((0,0),(w-1,h-1)) $ M.toList $ parse' ss
  where
    h = length ss
    w = length (head ss)

coordDiff :: Coord -> Int -> Coord -> Coord
coordDiff (x,y) m (dx,dy) = (x+m*dx, y+m*dy)

getsSun :: Grid -> Coord -> Set Coord
getsSun g dir = S.fromList
  [k | (k, h) <- U.assocs g, h>0
     , and [ h > g U.! p - d | d <- [1..9-h], let p = coordDiff k d dir, U.inRange (U.bounds g) p]
  ]

grow :: Grid -> Set Coord -> Grid
grow g sunny = U.accum (+) g $ (,1) <$> S.toList sunny 
              ++ [k | (k,h) <- U.assocs g, h>defaultValue, h <= 0]
  where
    step k h
      | h<=0 || k `S.member` sunny = h + 1
      | otherwise = h
      where

directions :: [Coord]
directions = [(-1,-1),(1,-1),(2,0),(1,1),(-1,1),(-2,0)]

army :: Grid -> Int
army startGrid = go startGrid 0 0 0
  where
    go g dix soldiers energy
      | soldiers>=100000 = energy
      | otherwise = go adjustedGrid ((dix+1) `mod` length directions) (soldiers + newSoldiers) (energy + length sunny)
      where
        dir = directions !! dix
        sunny = getsSun g dir
        newGrid = grow g sunny
        newSoldiers = length $ filter (==10) $ U.elems newGrid
        adjustedGrid = U.amap (\h -> if h == 10 then -2 else h) newGrid


day8 :: IO ()
day8 = do
  s1 <- getLines (8, 1)
  s2 <- getLines (8, 2)
  s3 <- getLines (8, 3)
  let mp1 = parse s1
      mp2 = parse s2
      mp3 = parse s3

  putStrLn $ "Quest8: part1: " ++ show (length $ getsSun mp1 (-1,-1))
  putStrLn $ "Quest8: part2: " ++ show (sum $ length . getsSun mp2 <$> directions)
  putStrLn $ "Quest8: part3: " ++ show (army mp3)
  return ()


-- First code - a bit slow, converted map to uarray above

getsSun' :: M.Map Coord Int -> Coord -> Set Coord
getsSun' mp dir = S.fromList
  [k | (k, h) <- M.toList mp, h>0
     , and [ h > c - d | d <- [1..9-h], Just c <- [M.lookup (coordDiff k d dir) mp]]
  ]


grow' :: M.Map Coord Int -> Set Coord -> M.Map Coord Int
grow' mp sunny = M.mapWithKey step mp
  where
    step k h
      | h<=0 || k `S.member` sunny = h + 1
      | otherwise = h
      where


army' :: M.Map Coord Int -> Int
army' startMap = go startMap 0 0 0
  where
    go mp dix soldiers energy
      | soldiers>=100000 = energy
      | otherwise = go adjustedMap ((dix+1) `mod` length directions) (soldiers + newSoldiers) (energy + length sunny)
      where
        dir = directions !! dix
        sunny = getsSun' mp dir
        newMap = grow' mp sunny
        newSoldiers = M.size $ M.filter (==10) newMap
        adjustedMap = M.map (\h -> if h == 10 then -2 else h) newMap



