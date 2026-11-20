{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Day19(day19) where

import Utils hiding (rt, lt)
import Data.Ord
import Data.List (permutations)
import Data.Bits


-- After the initial table (see below)
--            0   1   2   3   4   5   6   7
data Colour = W | C | M | B | Y | G | R | K deriving (Eq, Show, Ord, Read, Enum, Bounded)


-- I've seen this somewhere before 
-- Convert to and from bits to add
instance Semigroup Colour where
  (<>) :: Colour -> Colour -> Colour
  a <> b = toEnum (fromEnum a .|. fromEnum b)

instance Monoid Colour where
  mempty :: Colour
  mempty = W


parse :: [String] -> ([Colour], [Colour])
parse ls = (read . (:[]) <$> (concat $ cs!!0), read . (:[]) <$> (concat $ cs!!1))
  where
    cs = splitOn "" ls

parse' :: [String] -> ([Colour], [Char])
parse' ls = (read . (:[]) <$> (concat $ cs!!0), concat $ cs!!1)
  where
    cs = splitOn "" ls


findBest :: [Colour] -> [Char] -> Int
findBest w c = maximum ns
  where
    ns :: [Int]
    ns = product . stat1 . apply <$> ps
    ci :: [Int]
    ci = read . (:[]) <$> c
    ps = permutations [C, M, Y, K]
    apply :: [Colour] -> [Colour]
    apply p = zipWith (\a i -> a <> (p !! (i-1))) w ci



stat1 :: (Eq a, Ord a) => [a] -> [Int]
stat1 = take 3 . sortOn Down . (length <$>) <$> group . sort 


day19 :: IO ()
day19 = do
  s1 <- getLines (19, 1)
  s2 <- getLines (19, 2)
  s3 <- getLines (19, 3)

  let cs1 = parse s1
      cs2 = parse' s2
      cs3 = fst $ parse' s3
      window1 = zipWith (<>) (fst cs1) (snd cs1)
      best = findBest (fst cs2) (snd cs2)

      -- It must be possoble to make the resulting colours equal amounts
      -- This maximises the product
      availablePanes = sum $ stat1 cs3
      (eachColour, r) = availablePanes `quotRem` 3


  putStrLn $ "Quest19: part1: " ++ show (product $ stat1 window1)
  putStrLn $ "Quest19: part2: " ++ show (best)
  putStrLn $ "Quest19: part3: " ++ show (eachColour * (eachColour+1) * (eachColour+1))
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