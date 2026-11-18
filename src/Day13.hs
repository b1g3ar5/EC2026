{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day13(day13) where

import Utils 


div' :: (Real a,Integral b) => a -> a -> b
div' n d = floor ((toRational n) / (toRational d))


mod' :: (Real a) => a -> a -> a
mod' n d = n - (fromInteger f) * d 
  where
    f = div' n d


data Hand = Hand { positions :: Rational
                 , isClockwise :: Bool
                 , pos :: Rational
                 , change :: Rational
                 , life :: Int} deriving (Show, Eq, Ord)


parseHand :: [String] -> Hand
parseHand [p, c, s, n] = h {pos = pos h * 360 / positions h, change = chng}
  where
    h = Hand (fromInteger $ read p) (c=="CW") (fromInteger $ read s) 0 (fromInteger $ read n)
    chng = 360 / (positions h) * if (isClockwise h) then 1 else -1
parseHand ws = error $ "No parse: " ++ show ws


parse1 :: [String] -> [Hand]
parse1 ls = parseHand <$> xx
  where
    ds = [6,10,8,5] -- characters in the text description
    xx = transpose $ zipWith makeDrop ds ls    
    makeDrop n s = words $ (\c -> if c==':' then ' ' else c) <$> drop n s


parse2 :: [String] -> [[Hand]]
parse2 ls = parse1 <$> lss
  where
    lss = init <$> chunksOf 5 ls


move :: Hand -> Hand
move h = h { pos = (pos h + if (life h > 0) then change h else 0) `mod'` 360, life = life h - 1}


cross :: Hand -> Hand -> Bool
cross h g = dv /= 0 && x > 0.0 && x <= 1.00001
  where
    v k = if life k > 0 then change k else 0
    dv  = v g - v h
    num = ((pos h - pos g) * signum dv) `mod'` 360.0
    x :: Rational
    x   = num / (abs dv)


runClock :: [Hand] -> Int
runClock starts
  | null movers = 0
  | otherwise = q * sigsPerStep + sig2 + runClock hs2 --go maxlife starts 0
  where
    movers = filter ((>0) . life) starts
    minLife = minimum $ life <$> movers
    step = foldr lcm  1 $ round . positions <$> movers
    (q,r) = minLife `divMod` step -- We can do q steps of size step seconds

    -- Back to the start after step seconds
    (_, sigsPerStep) = go step starts 0
    hs1  = (\h -> h {life = life h - q * step}) <$> starts
    -- Do the remianing steps until the next clock dies
    (hs2, sig2) = go r hs1 0

    go :: Int -> [Hand] -> Int -> ([Hand], Int)
    go n hs sigs
      | n==0 = (hs, sigs)
      | otherwise = go (n-1) newhs (sigs + s)
      where
        newhs = move <$> hs
        s = length $ filter (\(i,j) -> cross (hs!!i) (hs!!j)) [(0,1), (0,2), (1,2)]


day13 :: IO ()
day13 = do
  s1 <- getLines (13, 1)
  s2 <- getLines (13, 2)
  s3 <- getLines (13, 3)
  let hs1 = parse1 s1
      hs2 = parse2 s2
      hs3 = parse2 s3


  putStrLn $ "Quest13: part1: " ++ show (runClock hs1)
  putStrLn $ "Quest13: part2: " ++ show (sum $ runClock <$> hs2)
  putStrLn $ "Quest13: part3: " ++ show (sum $ runClock <$> hs3)
  return ()

