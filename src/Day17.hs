{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TupleSections #-}

module Day17(day17) where

import Utils hiding (bfs)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Sequence (Seq)
import qualified Data.Sequence as Q
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Map as M


findPath :: Set Coord -> Int
findPath cs = bfs nextFn (0,0) (==finish)
  where
    finish = S.findMax cs
    nextFn s = filter (\n -> n `S.member` cs) $ neighbours4 s


bfs :: forall a. Ord a => (a -> [a]) -> a -> (a -> Bool) -> Int
bfs next start finish = go (S.singleton start) (Q.singleton (0,start))
  where
    --go :: Set a -> Seq (Int, a) -> Int
    go !seen = \case
                  Q.Empty -> error "No path"
                  (n,x) Q.:<| newq
                    | finish x -> n
                    | otherwise -> go (foldr S.insert seen ns) (newq Q.>< Q.fromList ((n+1,) <$> ns))
                    where
                      ns = filter (`S.notMember` seen) (next x)  


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
      grid2 = S.fromList $ fst <$> filter snd (parseGridWith (=='#') s2)
      grid3 = S.fromList $ fst <$> filter snd (parseGridWith (=='#') s3)
      sides = (\c -> 4 - length (filter (`S.member` grid2) $ neighbours4 c)) <$> (S.toList grid2)

  putStrLn $ "Quest17: part1: " ++ show (findPath grid1)
  putStrLn $ "Quest17: part2: " ++ show (sum sides)
  putStrLn $ "Quest17: part3: " ++ show (count3 grid3)
  return ()

