{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Day10(day10) where

import Utils 
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as I


data Ghost = Ghost {pos :: Coord, dir :: Coord, collisions :: Int} deriving (Eq, Ord, Show)
type Cave = Set Coord

parseGhost :: Char -> Coord
parseGhost '>' = (1,0)
parseGhost '<' = (-1,0)
parseGhost '^' = (0,-1)
parseGhost 'v' = (0,1)
parseGhost c = error $ "Not ghost: " ++ [c]


parse :: [String] -> (Cave, [Ghost])
parse ls = (cave, (\(p,d) -> Ghost p d 0) <$> gs)
  where
    css :: [(Coord, Char)]
    css = filter ((/= '#') . snd) $ concat $ (\(y, xs) -> (\(x,c) -> ((x,y), c)) <$> zip [0..] xs) <$> zip [0..] ls
    cave = S.fromList $ fst <$> css
    gs = (second parseGhost) <$> filter ((/= '.') . snd) css


move :: Cave -> Ghost -> Ghost
move cave g
  | (pos g + dir g) `S.member` cave = g {pos = pos g + dir g}
  | otherwise = g {dir = clockTurn $ dir g, collisions = collisions g + 1}


moves :: Cave -> Int -> Int -> [Ghost] -> Int --[Ghost]
moves cave n collisionMax ghosts
  | n==0 = sum $ collisions <$> ghosts
  | null newGhosts = n
  | otherwise = moves cave (n-1) collisionMax newGhosts
  where
    newGhosts = step cave collisionMax ghosts


-- One second: move, drop ghosts that have reached the limit, then reverse ghosts sharing a cell
step :: Cave -> Int -> [Ghost] -> [Ghost]
step cave collisionMax ghosts = concat $ processGroup <$> groups
  where
    newGhosts :: [Ghost]
    newGhosts = filter (\g -> collisions g < collisionMax) $ move cave <$> ghosts
    groups :: [[Ghost]]
    groups = groupBy (\a b -> pos a == pos b) $ sortOn pos newGhosts

    processGroup :: [Ghost] -> [Ghost]
    processGroup g
      | length g == 1 = g
      | otherwise = (\gh -> gh {dir = opp (dir gh), collisions = collisions gh + 1} ) <$> g


-- Make a map from the position and orientation of each ghost to the time and the ghosts
-- Then you can skip a lot of time when a position repeats
movesLong :: Cave -> Int -> [Ghost] -> Int
movesLong cave collisionMax = go 0 (Just M.empty) . sortGhosts
  where
    sortGhosts = sortOn (\g -> (pos g, dir g))
    key = fmap (\g -> (pos g, dir g))

    go :: Int -> Maybe (M.Map [(Coord, Coord)] (Int, [Ghost])) -> [Ghost] -> Int
    go t _ [] = t
    go t (Just seen) gs
      | Just (t0, priorgs) <- M.lookup (key gs) seen = skip t (t - t0) priorgs gs
    go t seen gs
      | length next < length gs = go (t + 1) (Just M.empty) next
      | otherwise = go (t + 1) (M.insert (key gs) (t, gs) <$> seen) next
      where
        next = sortGhosts $ step cave collisionMax gs

    -- Do as many loops as possible while ghosts stay alive
    skip :: Int -> Int -> [Ghost] -> [Ghost] -> Int
    skip t len priorgs ghosts
      | null perLoop = error "No ghost is gaining collisions: they will never all be removed"
      | otherwise = go (t + nLoops * len) Nothing newGhosts
      where
        collisionDiffs = zipWith (\a b -> collisions b - collisions a) priorgs ghosts
        perLoop = [ (collisions g, d) | (g, d) <- zip ghosts collisionDiffs, d > 0 ]
        -- Most loops that keep every ghost below the limit
        nLoops = minimum [ (collisionMax - 1 - c) `div` d | (c, d) <- perLoop ]
        newGhosts = zipWith (\g d -> g {collisions = collisions g + nLoops * d}) ghosts collisionDiffs


opp :: Coord -> Coord
opp (x,y) = (-x,-y)

day10 :: IO ()
day10 = do
  s1 <- getLines (10, 1)
  s2 <- getLines (10, 2)
  s3 <- getLines (10, 3)
  let --s2 = test
      (cave1, ghosts1) = parse $ s1
      (cave2, ghosts2) = parse $ s2
      (cave3, ghosts3) = parse $ s3
      n = 1000000

  putStrLn $ "Quest10: part1: " ++ show (moves cave1 10 100 ghosts1)
  putStrLn $ "Quest10: part2: " ++ show (n + 1 - moves cave2 n 10000 ghosts2)
  putStrLn $ "Quest10: part3: " ++ show (movesLong cave3 1000000000000 ghosts3)
  return ()

test = ["###########"
  , "#####.#####"
  , "###.....###"
  , "##.......##"
  , "##.>>.<<.##"
  , "#v...>...v#"
  , "##...v...##"
  , "##.......##"
  , "###.....###"
  , "#####.#####"
  , "###########"]
