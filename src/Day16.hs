{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Day16(day16) where

import Utils
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Foldable as F


parse :: String -> Seq (Char, Int)
parse cs = go cs
  where 
    go [] = S.Empty
    go [c] = S.singleton (c,1)
    go (c:r1) = (c, length cs + 1) S.<| go r2
      where
        (cs, r2) = span (==c) r1


sort1 :: Seq (Char, Int) -> (Seq (Char, Int), Int)
sort1 ts = go 0 ts
  where
    go :: Int -> Seq (Char, Int) -> (Seq (Char, Int) , Int)
    go count S.Empty = (S.Empty, count)
    go count ss
      | isNothing mindex = (first (\l -> (c,n) S.:<| l)) $ go count others
      | otherwise = go (count + 1) nxt
      where
        (c,n) S.:<| others = ss
        mindex = S.findIndexR ((==c) . fst) others
        (notCs, (_, newN) S.:<| remaining) = S.splitAt (fromJust mindex) others
        nxt = ((c , n + newN)) S.:<| join (S.reverse notCs)  remaining        


join :: Seq (Char, Int) -> Seq (Char, Int) -> Seq (Char, Int)
join x y
  | S.null x = y
  | S.null y = x
  | cx==cy = xs S.>< ((cx, nx+ny) S.:<| ys)
  | otherwise = x S.>< y
  where
    (xs S.:|> (cx, nx)) = x
    ((cy, ny) S.:<| ys) = y


-- Don't do any reversing just alternate ends ...
count :: Seq Char -> Int
count = go . F.toList
  where
    go [] = 0
    go (c : rest) = cost + go newRest
      where
        blocks = groupBy ((==) `on` (== c)) (dropWhile (== c) rest)
        m = length blocks `div` 2
        (body, end) = splitAt (2 * m) blocks
        cost = sum (zipWith3 (\b x y -> length b * min x y) body [1 ..] [2 * m, 2 * m - 1 .. 1])
        gaps = evens body
        newRest = concat (reverse (take m (interleave (map reverse gaps) (reverse gaps))))
              ++ concat end

    evens (x : _ : xs) = x : evens xs
    evens _ = []

    interleave (x : xs) ys = x : interleave ys xs
    interleave [] ys = ys


day16 :: IO ()
day16 = do
  s1 <- getLines (16, 1)
  s2 <- getLines (16, 2)
  s3 <- getLines (16, 3)

  let tablets1 = parse $ head s1
      tablets2 = S.fromList $ concat $ replicate 10 $ head s2
      tablets3 = S.fromList $ concat $ replicate 100 $ head s3

  putStrLn $ "Quest16: part1: " ++ show (snd $ sort1 $ tablets1)
  putStrLn $ "Quest16: part2: " ++ show (count tablets2)
  putStrLn $ "Quest16: part3: " ++ show (count tablets3)
  return ()



--- GRAVEYARD - First attempt, gets the right answer but complicated!

{-
-- An attempt at improving the speed!
data Colours = C {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
data TMeasure = TM { tmRuns :: !Int, tmTabs :: !Int, tmCols :: {-# UNPACK #-} !Colours }


instance Semigroup TMeasure where
  (<>) :: TMeasure -> TMeasure -> TMeasure
  TM r1 t1 (C a1 b1 c1 d1) <> TM r2 t2 (C a2 b2 c2 d2) =
    TM (r1+r2) (t1+t2) (C (a1+a2) (b1+b2) (c1+c2) (d1+d2))


instance Monoid TMeasure where
  mempty :: TMeasure
  mempty = TM 0 0 (C 0 0 0 0)


colourVec :: Char -> Colours
colourVec 'R' = (C 1 0 0 0)
colourVec 'G' = (C 0 1 0 0)
colourVec 'B' = (C 0 0 1 0)
colourVec 'Y' = (C 0 0 0 1)
colourVec c   = error ("colourVec: " ++ [c])

colourCount :: Char -> TMeasure -> Int
colourCount c (TM _ _ (C r g b y)) = 
  case c of
    'R' -> r; 'G' -> g; 'B' -> b; 'Y' -> y
    _   -> error "colourCount"


instance T.Measured TMeasure (Char, Int) where
  measure :: (Char, Int) -> TMeasure
  measure (c, n) = TM 1 n (colourVec c)


type Tablets = (FingerTree TMeasure (Char, Int), FingerTree TMeasure (Char, Int))


makeTablets :: Seq (Char, Int) -> Tablets
makeTablets s = (T.fromList xs, T.fromList $ reverse xs)
  where
    xs = F.toList s


sortTablets :: Tablets -> Int
sortTablets ts = go 0 ts
  where
    go :: Int -> Tablets -> Int
    go count tabs@(fwd, bwd)
      | T.null fwd = count
      | mindex == 0 = go count (fothers, bothers)
      | otherwise = go (count + newCount + newN) ((c, n + newN) T.<| (bnotCs `join2` fremaining)
                                          , (bremaining `join2` fnotCs) T.|> (c, n + newN)
                                          )
      where
        (c,n) T.:< fothers = T.viewl fwd        
        bothers T.:> _ = T.viewr bwd
        mindex = colourCount c (T.measure fothers) 
        (fnotCs, frest) = T.split (\m -> colourCount c m >= mindex) fothers
        (bremaining, bnotCsp) = T.split (\m -> colourCount c m >= 1) bothers
        ((c', newN) T.:< fremaining) = T.viewl frest
        (_ T.:< bnotCs) = T.viewl bnotCsp
        newCount = tmTabs (T.measure fnotCs)

join2 :: FingerTree TMeasure (Char, Int) -> FingerTree TMeasure (Char, Int) -> FingerTree TMeasure (Char, Int)
join2 x y
  | T.null x = y
  | T.null y = x
  | cx==cy = xs T.>< ((cx, nx+ny) T.<| ys)
  | otherwise = x T.>< y
  where
    (xs T.:> (cx, nx)) = T.viewr x
    ((cy, ny) T.:< ys) =T.viewl y

-}

