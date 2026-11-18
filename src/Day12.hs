{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day12(day12) where

import Utils 
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Map (Map)


parse :: [String] -> [Coord]
parse ls = fst <$> 
           filter ((=='#') . snd) (concat $ (\(y,l) -> (\(x,c) -> (((x,y), c) )
                                                       ) <$> zip [0..] l
                                            ) <$> zip [0..] ls
                                  )


findShapes :: [String] -> [Set Coord]
findShapes css = mkShapes $ S.fromList allCoords
  where
    allCoords :: [Coord]
    allCoords = fst <$> filter ((=='#').snd) (concat $ (\(y,cs) -> (\(x,c) -> ((x,y),c)) <$> zip [0..] cs) <$> zip [0..] css)

    -- Groups the coords into contiguous shapes
    mkShapes :: Set Coord -> [Set Coord]
    mkShapes startcs
      | S.null startcs = []
      | otherwise = cg : mkShapes rem
      where
        Just (c,cs) = S.minView startcs
        (cg, rem) = mkShape c cs

    -- finds all coords contiguous with c
    mkShape :: Coord -> Set Coord -> (Set Coord, Set Coord)
    mkShape c cs = go (S.singleton c) cs
      where
        go ps others
          | S.null nxt = (ps,others)
          | otherwise = (\(s,r) -> (ps `S.union` s,r)) $ go nxt (others S.\\ nxt)
          where
            nxt = S.filter (`S.member` others) $ S.unions (S.map (S.fromList . neighbours4) ps)


shapeMap :: [Coord] -> (Map Int [Int], Map Int [Int])
shapeMap cs = (M.fromList xs, M.fromList ys)
  where
    xs, ys :: [(Int, [Int])]
    xs = (\g -> (fst $ head g, snd <$> g)) <$> groupBy (\a b -> fst a == fst b) (sortOn fst cs)
    ys = (\g -> (snd $ head g, fst <$> g)) <$> groupBy (\a b -> snd a == snd b) (sortOn snd cs)


-- a is the id of a line, the [b] are the Coords of the cells covered
maxMatching :: forall a b. (Ord a, Ord b) => Map a [b] -> Int
maxMatching adj = M.size $ foldl' step M.empty (M.keys adj)
  where
    step :: Map b a -> a -> Map b a
    step m u = either (const m) snd (augment u S.empty m)
    augment :: a -> Set b -> Map b a -> Either (Set b) (Set b, Map b a)
    augment u vis m = go (adj M.! u) vis
      where
        go :: [b] -> Set b -> Either (Set b) (Set b, Map b a)
        go [] vis' = Left vis'
        go (v:vs) vis'
          | v `S.member` vis' = go vs vis'
          | otherwise = case M.lookup v m of
                          Nothing -> Right (S.insert v vis', M.insert v u m)
                          Just u' -> case augment u' (S.insert v vis') m of
                                        Right (vis'', m') -> Right (vis'', M.insert v u m')
                                        Left vis''        -> go vs vis''


-- Map of all the lines in the shape
-- horizontal line id = (y, start x of its run); vertical line id = (x, start y of its run)
segmentMap :: [Coord] -> Map Coord [Coord]
segmentMap cs = M.fromListWith (++) [ ((y, hStart M.! c), [(x, vStart M.! c)]) | c@(x,y) <- cs ]
  where
    -- scan each row left to right: copy the start from the left neighbour, else this cell starts a run
    hStart = foldl' (\m c@(x,y) -> M.insert c (M.findWithDefault x (x-1,y) m) m) M.empty
                    (sortOn (\(x,y) -> (y,x)) cs)
    -- scan each column top to bottom in the same way
    vStart = foldl' (\m c@(x,y) -> M.insert c (M.findWithDefault y (x,y-1) m) m) M.empty
                    (sort cs)


day12 :: IO ()
day12 = do
  s1 <- getLines (12, 1)
  s2 <- getLines (12, 2)
  s3 <- getLines (12, 3)
  let ss1 = findShapes s1
      ss2 = findShapes s2
      ss3 = findShapes s3


      x2 = maxMatching . fst . shapeMap . S.toList <$> ss2
      x3 = maxMatching (segmentMap $ parse s3) -- just treat the whole thing as a shape!

  putStrLn $ "Quest12: part2: " ++ show (sum x2)
  putStrLn $ "Quest12: part3: " ++ show (x3)
  return ()


test = ["........#."
  , ".##......."
  , ".##..####."
  , ".##..####."
  , ".....####."
  , ".###......"]

test2 = ["..............................."
  , ".####....#####..#######........"
  , ".####....#####..#######........"
  , ".####....#####..#######........"
  , ".........#####.......##........"
  , ".........########....##........"
  , ".........########....##........"
  , "..............................."
  , ".######....#.....#.....###....."
  , "...........#...........###....."
  , "...........#......########....."
  , "...........#......########....."
  , "...........#......########....."
  , "..................########....."
  , "..............................."]  

test3 = [".............................."
  , ".############################."
  , ".##.......................###."
  , ".##..################.###.###."
  , ".##..######.........#.###.###."
  , ".##..################.###.###."
  , ".##...................###.###."
  , ".##..####################.###."
  , ".##..####################.###."
  , ".##.......................###."
  , ".############################."
  , ".............................."]  