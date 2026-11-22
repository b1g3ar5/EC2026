{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Day20(day20) where

import Utils hiding (rt, lt)
import Control.Monad

-- There must be a quicker way to do this using the dirrerence between the
-- two sides of the equation - but it's quick enough here just trying
-- changes until one works.

type Side = [String] 
type Eqn = [String]


parse11 :: String -> Int
parse11 cs = if ((op $ (head (ws!!1))) (read11 $ ws!!0) (read11 $ ws!!2)) 
             == ((op $ (head (ws!!5))) (read11 $ ws!!4) (read11 $ ws!!6)) 
              then 
                sum $ segments <$> (filter (/=' ') cs)
              else 
                0
  where
    ws = words cs


read11 :: String -> Int
read11 s
  | s == "+" = -1
  | s == "-" = -2
  | otherwise = go $ reverse s
  where
    go [] = 0
    go (c:cs) = (if c == 'A' then 10 else (read [c])) + (11 * go cs)


show11 :: Int -> String
show11 n
  | n == (-1) = "+"
  | n == (-2) = "-"
  | n<10 = show n
  | n==10 = "A"
  | otherwise = show11 q ++ show11 r
    where
      (q,r) = n `quotRem` 11

op :: Char -> (Int -> Int -> Int)
op '+' = (+)
op '-' = (-)
op _ = error $ "Must be + or -"


-- Segments used in the calculator display
segments :: Char -> Int
segments '0' = 6
segments '1' = 2
segments '2' = 5
segments '3' = 5
segments '4' = 4
segments '5' = 5
segments '6' = 6
segments '7' = 3
segments '8' = 7
segments '9' = 6
segments 'A' = 6
segments '+' = 2
segments '-' = 1
segments '=' = 2
segments c = error $ "Error: " ++ show c

-- Potential moves for a number
selfMoves, addMoves, subMoves :: Int -> [Int]
selfMoves 0 = [0,6,9,10]
selfMoves 1 = [1]
selfMoves 2 = [2,3]
selfMoves 3 = [2,5]
selfMoves 4 = [4]
selfMoves 5 = [3,5]
selfMoves 6 = [0,6,9,10]
selfMoves 7 = [7]
selfMoves 8 = [8]
selfMoves 9 = [0,6,9,10]
selfMoves 10 = [0,6,9,10]
selfMoves _ = []

addMoves 0 = [8]
addMoves 1 = [7]
addMoves 3 = [9]
addMoves 5 = [6,9]
addMoves 6 = [8]
addMoves 9 = [8]
addMoves 10 = [8]
addMoves (-2) = [-1]
addMoves _ = []

subMoves 8 = [0,10,6,9]
subMoves 7 = [1]
subMoves 9 = [3,5]
subMoves 6 = [5]
subMoves (-1) = [-2]
subMoves _ = []


value :: Side -> Int
value [a, "+", b] = read11 a + read11 b
value [a, "-", b] = read11 a - read11 b
value s = error $ "Only 2 ops, -1 and -2: " ++ show s


diff :: Eqn -> Int
diff s = value (take 3 s) - value (drop 3 s)
 

totalSegments :: Eqn -> Int
totalSegments s
  | length s /= 6 = error $ "Not an equation" ++ show s
  | otherwise = 2 + sum (segments <$> (s!!1 ++ s!!0 ++ s!!2 
                                    ++ s!!4 ++ s!!3 ++ s!!5))

parseSeg :: String -> Maybe Int
parseSeg s
  | diff eq == 0 = Just $ totalSegments eq
  | null (getAddSubMoves ++ getSelfMoves) = Nothing
  | otherwise = Just $ totalSegments $ head $ getAddSubMoves ++ getSelfMoves
  where
    ws = words s
    eq = [ws!!0, ws!!1, ws!!2, ws!!4, ws!!5, ws!!6]
    eqStr = concat eq

    getAddSubMoves, getSelfMoves :: [Eqn]
    getAddSubMoves = do
      let n = length eqStr
      ix1 <- [0..n-1]
      ix2 <- [0..n-1]
      guard $ ix1 /= ix2
      addMove <- addMoves (read11 $ [eqStr !! ix1])
      subMove <- subMoves (read11 $ [eqStr !! ix2])
      let newEq = update ix2 (head $ show11 subMove) $ update ix1 (head $ show11 addMove) eq
      guard $ diff newEq == 0
      return newEq

    getSelfMoves = do
      let n = length eqStr
      ix1 <- [0..n-1]
      move <- selfMoves (read11 $ [eqStr !! ix1])
      let newEq = update ix1 (head $ show11 move) eq
      guard $ diff newEq == 0
      return newEq


    -- Incorpoate the changed character into the equation
    update :: Int -> Char -> Eqn -> Eqn
    update _ _ [] = error $ "ix is too big?"
    update ix c (s:ss)  
      | ix >= length s = s : update (ix - length s) c ss
      | otherwise = (zipWith (\i x -> if i==ix then c else x) [0..] s) : ss


day20 :: IO ()
day20 = do
  s1 <- getLines (20, 1)
  s2 <- getLines (20, 2)
  s3 <- getLines (20, 3)

  let cs1 = parse11 <$> s1
      cs2 = parse11 <$> s2

  putStrLn $ "Quest20: part1: " ++ show (sum cs1)
  putStrLn $ "Quest20: part2: " ++ (show11 $ sum cs2)
  putStrLn $ "Quest20: part3: " ++ show11 (sum $ catMaybes $ parseSeg <$> s3)
  return ()

