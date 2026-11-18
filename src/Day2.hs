module Day2(day2) where

import qualified Utils as U
import Utils hiding (sort)
import Data.Set (Set)
import qualified Data.Set as S


sort :: [Int] -> (Int, Int) -> [Int]
sort [] _ = error "Empty list"
sort [x] _ = [x]
sort lst@(x:y:ys) (h, l) 
  | x==h = lst
  | x==l = h : l : filter (/=h) (y:ys)
  | otherwise = x : sort (y:ys) (h, l)


parseSwaps:: String -> Set (Int, Int)
parseSwaps ss = S.fromList $ U.sort $ go <$> ws
  where
    go :: String -> (Int, Int)
    go wd
      | wd!!1 == '>' = (read $ [wd!!0], read $ [wd!!2])
      | wd!!1 == '<' = (read $ [wd!!2], read $ [wd!!0])
      | otherwise = error "Invalid swap format"
    ws = words ss


toInt :: [Int] -> Int
toInt [] = error "Empty list"
toInt [x] = x
toInt (x:xs) = x + 10 * toInt xs


-- Number of rules each item wins
wins :: [(Int, Int)] -> Int -> Int
wins swaps n = length $ filter ((==n) . fst) swaps


-- Order by number of wins; exact when every pair is present
order :: [(Int, Int)] -> [Int]
order swaps = sortOn (negate . wins swaps) $ nub $ concatMap (\(a,b) -> [a,b]) swaps


closure :: Set (Int, Int) -> Set (Int, Int)
closure swaps
  | S.size next == S.size swaps = swaps
  | otherwise = closure next
  where
    next = S.union swaps $ S.fromList [(a,d)| (a,b)<-S.toList swaps, (c,d)<-S.toList swaps, b==c]


day2 :: IO ()
day2 = do
  s1 <- getLines (2, 1)
  s2 <- getLines (2, 2)
  s3 <- getLines (2, 3)
  let swaps1 = parseSwaps <$> s1
      swaps2 = parseSwaps <$> s2
      swaps3 = parseSwaps <$> s3
      aug2 = closure <$> swaps2
      aug3 = closure <$> swaps3
  putStrLn $ "Quest2: part1: " ++ show (sum $ toInt . reverse <$> foldl sort [1,2,3,4,5] <$> swaps1)
  putStrLn $ "Quest2: part2: " ++ show (sum $ toInt . reverse . order . S.toList <$> aug2)
  putStrLn $ "Quest2: part3: " ++ show (sum $ toInt . reverse . order . S.toList <$> aug3)
  return ()

