module Day4(day4) where

import Data.Ord
import Utils 

data Task = Task {ix :: Int, qt :: Int, pt :: Int, tm :: Int, sc :: Int} deriving (Eq, Ord, Show)

parse :: String -> Task
parse s = Task (xs $ ws!!0) (xs $ ws!!1) (xs $ ws!!2) (xs $ ws!!3) 0
  where
    ws = words s
    xs w = read $ (splitOn '=' w) !! 1

partScore :: [Int]
partScore = [50, 100, 150]

-- Work out the score for a (quest, part) and add it to the Task{}
addScore :: (Int, Int) -> [Task] -> [Task]
addScore (quest, part) ts = go (partScore !! (part-1) + 1, partScore !! (part-1)) 0 scores
  where
    fs = filter (\t -> qt t == quest && pt t <= part) ts
    -- Must have scored on all earlier tasks
    byID = filter (\g -> length g == part) $ groupBy (\a b -> ix a == ix b) fs
    scores = sortOn tm $ (foldr1 (\s t -> s {tm = tm s + tm t })) <$> byID
    
    go _ _ [] = []
    go (0,_) _ xs = xs  -- if score is 0 then so are all subsequent
    go (_,-1) _ xs = xs -- if the next score is -1 then all zeros
    go (lastScore, nextScore) lastTime (x:xs)
      | (tm x) == lastTime = x {sc=lastScore} : go (lastScore, nextScore-1) (tm x) xs
      | otherwise = x {sc=nextScore} : go (nextScore, nextScore-1) (tm x) xs


day4 :: IO ()
day4 = do
  s1 <- getLines (4, 1)
  s2 <- getLines (4, 2)
  s3 <- getLines (4, 3)
  let a1 = sum $ zipWith (*) [1..] $ sort $ 
           sum <$> (tm <$>) <$> groupBy (\a b -> ix a == ix b) (sort $ parse <$> s1)

      a2 = sum $ zipWith (\a b -> a* snd b) [1..]
         $ sortOn (\(x, y) -> (Down x, y))
         $ (\g -> (length g, sum $ tm <$> g)) <$> groupBy (\a b -> ix a == ix b) (sort $ parse <$> s2) 

      z3 = sort $ parse <$> s3 
      qps = sort $ nub $ (\x -> (qt x, pt x)) <$> z3
      a3 = foldl (\(acc, nxt) g -> (acc + nxt*(g!!0)*(length g), nxt + length g)) (0, 1) 
         $ group $ sortOn Down $ (sum . (sc <$>)) 
         <$> groupBy (\a b -> ix a == ix b) (sort $ concatMap (`addScore` z3) qps)

  putStrLn $ "Quest4: part1: " ++ show a1
  putStrLn $ "Quest4: part2: " ++ show a2
  putStrLn $ "Quest4: part3: " ++ show (fst a3)
  return ()



test1 = ["id=1 quest=1 part=1 time=187"
  , "id=3 quest=1 part=1 time=413"
  , "id=5 quest=1 part=3 time=744"
  , "id=2 quest=1 part=3 time=745"
  , "id=4 quest=1 part=2 time=558"
  , "id=1 quest=1 part=2 time=299"
  , "id=1 quest=1 part=3 time=362"
  , "id=4 quest=1 part=3 time=1268"
  , "id=4 quest=1 part=1 time=457"
  , "id=5 quest=1 part=1 time=257"
  , "id=2 quest=1 part=1 time=192"
  , "id=3 quest=1 part=3 time=1168"
  , "id=5 quest=1 part=2 time=346"
  , "id=3 quest=1 part=2 time=455"
  , "id=2 quest=1 part=2 time=562"]

test2 = ["id=2 quest=1 part=1 time=100"
  , "id=4 quest=2 part=1 time=150"
  , "id=1 quest=2 part=1 time=140"
  , "id=1 quest=1 part=2 time=180"
  , "id=2 quest=2 part=1 time=130"
  , "id=4 quest=1 part=1 time=110"
  , "id=1 quest=2 part=2 time=210"
  , "id=2 quest=2 part=2 time=240"
  , "id=1 quest=2 part=3 time=330"
  , "id=2 quest=1 part=2 time=170"
  , "id=3 quest=1 part=1 time=90"
  , "id=4 quest=1 part=2 time=175"
  , "id=3 quest=2 part=1 time=120"
  , "id=1 quest=1 part=1 time=120"
  , "id=3 quest=2 part=2 time=220"
  , "id=2 quest=2 part=3 time=350"
  , "id=3 quest=1 part=2 time=160"]

test3 = ["id=2 quest=1 part=1 time=90"
  , "id=4 quest=2 part=1 time=150"
  , "id=1 quest=2 part=1 time=130"
  , "id=1 quest=1 part=2 time=180"
  , "id=2 quest=2 part=1 time=130"
  , "id=4 quest=1 part=1 time=110"
  , "id=1 quest=2 part=2 time=210"
  , "id=2 quest=2 part=2 time=220"
  , "id=1 quest=2 part=3 time=330"
  , "id=2 quest=1 part=2 time=170"
  , "id=3 quest=1 part=1 time=90"
  , "id=4 quest=1 part=2 time=170"
  , "id=3 quest=2 part=1 time=120"
  , "id=1 quest=1 part=1 time=120"
  , "id=3 quest=2 part=2 time=220"
  , "id=2 quest=2 part=3 time=350"
  , "id=3 quest=1 part=2 time=170"]


test3_2 = ["id=2 quest=1 part=1 time=100"
  , "id=4 quest=2 part=1 time=150"
  , "id=1 quest=2 part=1 time=140"
  , "id=1 quest=1 part=2 time=180"
  , "id=2 quest=2 part=1 time=130"
  , "id=4 quest=1 part=1 time=110"
  , "id=1 quest=2 part=2 time=210"
  , "id=2 quest=2 part=2 time=240"
  , "id=1 quest=2 part=3 time=330"
  , "id=2 quest=1 part=2 time=170"
  , "id=3 quest=1 part=1 time=90"
  , "id=4 quest=1 part=2 time=175"
  , "id=3 quest=2 part=1 time=120"
  , "id=1 quest=1 part=1 time=120"
  , "id=3 quest=2 part=2 time=220"
  , "id=2 quest=2 part=3 time=350"
  , "id=3 quest=1 part=2 time=160"]


