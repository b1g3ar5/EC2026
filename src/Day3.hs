{-# LANGUAGE TupleSections #-}

module Day3(day3) where

import Utils hiding (sort)


parse :: Int -> String -> ([Int], [Int])
parse dp s = (m1, m2)
  where
    ws = words (drop dp s)
    m1, m2 :: [Int]
    m1 = read . (:[]) <$> ws!!0
    m2 = read . (:[]) <$> ws!!1


value1 :: String -> Int
value1 s =  sum $ zipWith (\p (a,b) -> b*(a^p)) [3,2,1,0] (zip n1 n2)
  where
    n1, n2 :: [Int]
    n1 = read . (:[]) <$> take 4 (drop 4 s)
    n2 = read . (:[]) <$> drop 9 s


weights :: [Int] -> [Int]
weights bs = zipWith (^) (cycle (reverse bs)) [0 :: Int ..]

value2 :: ([Int], [Int]) -> Int
value2 (m1,m2) =  sum $ zipWith (\p (a,b) -> b*(a^p)) (reverse [0..(l-1)]) (zip n1 n2)
  where
    n1, n2 :: [Int]
    l = length m2
    c1 = l `div` length m1
    n1 = (drop ((c1+1) * length m1 - l) m1) ++ concat (replicate c1 m1)
    n2 = m2


isValid :: ([Int], [Int]) -> Bool
isValid t@(bs, ms) = smallest (value2 t) ws caps == Just ms
  where
    ws   = reverse (take (length ms) (weights bs))
    caps = drop 1 (scanr (\w acc -> acc + 9 * w) 0 ws)


firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust ((Just y):_) = Just y
firstJust (Nothing:xs) = firstJust xs


smallest :: Int -> [Int] -> [Int] -> Maybe [Int]
smallest r [] _ = if r == 0 then Just [] else Nothing
smallest r (w:ws) (cap:caps) =
  firstJust [ (d :) <$> smallest r' ws caps
            | d <- [0 .. 9], let r' = r - d*w, r' >= 0, r' <= cap ]
smallest _ _ [] = Nothing


day3 :: IO ()
day3 = do
  s1 <- getLines (3, 1)
  s2 <- getLines (3, 2)
  s3 <- getLines (3, 3)

  let ts = parse 5 <$> s3

  putStrLn $ "Quest3: part1: " ++ show (sum $ value1 <$> s1)
  putStrLn $ "Quest3: part2: " ++ show (sum $ value2 . parse 4 <$> s2)
  putStrLn $ "Quest3: part3: " ++ show (sum $ (\t -> if isValid t then value2 t else 0) <$> ts)
  return ()


test2 = ["ISO-3210 7654"
  , "ISO-8742 2468"
  , "ISO-1 1111"
  , "ISO-13 777"
  , "ISO-34 140856"
  , "ISO-789 2465112"
  , "ISO-2341 945"]


test3 = ["ISO2-3 23"
  , "ISO2-61 149"
  , "ISO2-978 99"
  , "ISO2-5887 5923"
  , "ISO2-76988 123749"
  , "ISO2-435657 929977"
  , "ISO2-3949 870737"]


