{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Day19(day19) where

import Utils hiding (rt, lt)
import Data.Ord
import Data.List (permutations)
import Data.Bits


-- A ring with 8 elements, commutative, zero = K
-- Fits the addition table given in the problem
data Colour = W | C | M | B | Y | G | R | K deriving (Eq, Show, Ord, Read, Enum, Bounded)
--            0   1   2   3   4   5   6   7

-- Convert to and from bits, <> == or/union
instance Semigroup Colour where
  (<>) :: Colour -> Colour -> Colour
  a <> b = toEnum (fromEnum a .|. fromEnum b)

instance Monoid Colour where
  mempty :: Colour
  mempty = W


parse1 :: [String] -> ([Colour], [Colour])
parse1 ls = (read . (:[]) <$> (concat $ cs!!0), read . (:[]) <$> (concat $ cs!!1))
  where
    cs = splitOn "" ls

parse2 :: [String] -> ([Colour], [Char])
parse2 ls = (read . (:[]) <$> (concat $ cs!!0), concat $ cs!!1)
  where
    cs = splitOn "" ls


findBest :: [Colour] -> [Char] -> Int
findBest w c = maximum $ product . frequencies . apply <$> ps
  where
    ci :: [Int]
    ci = read . (:[]) <$> c
    ps = permutations [C, M, Y, K]
    apply :: [Colour] -> [Colour]
    apply p = zipWith (\a i -> a <> (p !! (i-1))) w ci


-- Top 3 colours
frequencies :: (Eq a, Ord a) => [a] -> [Int]
frequencies = take 3 . sortOn Down . (length <$>) <$> group . sort 


day19 :: IO ()
day19 = do
  s1 <- getLines (19, 1)
  s2 <- getLines (19, 2)
  s3 <- getLines (19, 3)

  let cs1 = parse1 s1
      cs2 = parse2 s2
      cs3 = fst $ parse2 s3
      window1 = zipWith (<>) (fst cs1) (snd cs1)
      bestWindow2 = findBest (fst cs2) (snd cs2)

      -- It must be possible to make the resulting colours equal amounts
      -- so that we maximise the product
      availablePanes = sum $ frequencies cs3
      (q, r) = availablePanes `quotRem` 3
      bestWindow3 = product cols
        where
          cols
            | r==0 = [q,q,q]
            | r==1 = [q+1,q,q]
            | r==2 = [q+1,q+1,q]
            | otherwise = error "This is not possible"


  putStrLn $ "Quest19: part1: " ++ show (product $ frequencies window1)
  putStrLn $ "Quest19: part2: " ++ show (bestWindow2)
  putStrLn $ "Quest19: part3: " ++ show (bestWindow3)
  return ()



{-
-- I should figure out how to have commutativity and a zero element.
-- Monoid has a unit but it's an additive zero not multiplicative
instance Semigroup Colour where
  (<>) :: Colour -> Colour -> Colour
  C <> M = B
  C <> Y = G
  Y <> M = R

  M <> C = B
  Y <> C = G
  M <> Y = R

  K <> _ = K
  _ <> K = K

  C <> C = C
  M <> M = M
  Y <> Y = Y
  a <> b = error $ "I don't know how to multiply " ++ show (a,b)
-}


test = ["CCCCC"
  , "MMMMM"
  , "YYYYY"
  , "KKKKK"
  , "CCMYY"
  , ""
  , "CMYKC"
  , "CMYKC"
  , "CMYKC"
  , "CMYKC"
  , "CMYKC"]

test2 = ["CCCCC"
  , "MMMMM"
  , "YYYYY"
  , "KKKKK"
  , "CCMYY"
  , ""
  , "14231"
  , "14231"
  , "14231"
  , "14231"
  , "14231"]