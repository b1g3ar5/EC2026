{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Day17(day17) where

import Utils
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Map as M


findPath :: Set Coord -> Int
findPath cs = (dijkstra pipeline visited nextFn costFn finishFn) M.! finish 
  where
    finish = S.findMax cs
    pipeline = PQ.singleton 0 (0,0)
    visited = M.empty 
    nextFn s = filter (\n -> n `S.member` cs) $ neighbours4 s
    costFn _ _ = 1
    finishFn = (==finish)


count3 :: Set Coord -> Int
count3 cs = min (dnPath+1) (rtPath+2) -- adding the 3 edges of the finish tile
  where
    finish = S.findMax cs
    dnPath = go antiTurn ((0,0), dn, lt, 0) 
    rtPath = go clockTurn ((0,0), rt, up, 0)
    go :: (Coord -> Coord) -> (Coord, Coord, Coord, Int) -> Int
    go turn (pos, dir, side, count)
      | pos == finish = count
      | nxtPos `S.notMember` cs = go turn (pos, turn dir, turn side, count+1)
      | nxtSide `S.member` cs = go turn (nxtSide, anti dir, anti side, count+1)
      | otherwise = go turn (nxtPos, dir, side, count+1)
      where        
        anti = (\(x,y) ->(-x,-y)) . turn 
        nxtPos = pos + dir
        nxtSide = nxtPos + side       


day17 :: IO ()
day17 = do
  s1 <- getLines (17, 1)
  s2 <- getLines (17, 2)
  s3 <- getLines (17, 3)
  let grid1 = S.fromList $ fst <$> filter snd (parseGridWith (=='#') s1)
      grid2 = fst <$> filter snd (parseGridWith (=='#') s2)
      grid3 = S.fromList $ fst <$> filter snd (parseGridWith (=='#') s3)
      sides = (\c -> 4 - length (filter (`elem` grid2) $ neighbours4 c)) <$> grid2

  putStrLn $ "Quest17: part1: " ++ show (findPath grid1)
  putStrLn $ "Quest17: part2: " ++ show (sum sides)
  putStrLn $ "Quest17: part3: " ++ show (count3 grid3)
  return ()



test1 = ["####.######"
  , "#.#.##..#.."
  , "##.##.##..#"
  , "#.#.###.###"
  , "#.###.###.#"
  , "###.##.#.##"]