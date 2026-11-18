{-# LANGUAGE FlexibleContexts #-}

module Day5(day5) where

import Utils 
import Data.Set (Set)
import qualified Data.Set as S
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad
import Control.Monad.ST (ST)

max1,max2, max3 :: Coord
max1 = (101, 21)
max2 = (201, 201)
max3 = (299, 163)

getOnes :: [String] -> [Coord]
getOnes ss = fst <$> filter snd (concat $ (\(y, xs) -> (\(x, c) -> ((x,y), c=='1')) 
           <$> zip [0::Int ..] xs) <$> zip [0::Int ..] ss)

getStart :: [String] -> Coord
getStart ss = (length (ss!!0) `div` 2, length ss `div` 2)

sizeArray :: UArray Coord Bool -> Int
sizeArray  = length . filter id . elems


-- Grow a region from seed cells, one round at a time. A cell joins when it
-- is allowed and has at least threshold neighbours that are either already
-- in the region or fixed. Stops after the given number of rounds, or when
-- nothing new joins.
{-# INLINE grow #-}
grow :: (Coord, Coord) -> Int -> (Coord -> Bool) -> (Coord -> Bool) -> Int -> [Coord]
     -> UArray Coord Bool
grow bnds rounds allowed fixed threshold seeds = runSTUArray $ do
  inR <- newArray bnds False
  seen <- newSeen bnds
  let okNeighbours = filter (inRange bnds) . neighbours8
      -- each candidate is checked at most once per round
      firstVisit n p = do
        v <- readArray seen p
        if v == n then pure False else writeArray seen p n >> pure True
      counts p = if fixed p then pure True else readArray inR p
      eligible p = do
        r <- readArray inR p
        if r || not (allowed p)
        then pure False
        else (>= threshold) . length <$> filterM counts (okNeighbours p)
      loop 0 _  = pure ()
      loop _ [] = pure ()
      loop n frontier = do
        newFrontier <- filterM (\p -> (&&) <$> firstVisit n p <*> eligible p)
                               (concatMap okNeighbours frontier)
        forM_ newFrontier $ \p -> writeArray inR p True
        loop (n-1) newFrontier
  forM_ seeds $ \p -> writeArray inR p True
  loop rounds seeds
  pure inR


newSeen :: (Coord, Coord) -> ST s (STUArray s Coord Int)
newSeen bnds = newArray bnds 0


growLand :: (Coord, Coord) -> UArray Coord Bool -> Coord -> UArray Coord Bool
growLand bnds ones start = grow bnds maxBound (const True) (ones !) 5 [start]


growSand :: (Coord, Coord) -> UArray Coord Bool -> UArray Coord Bool
growSand bnds land = grow bnds maxBound (land !) (const False) 5 startSand
  where
    hasWater = any (not . (land !)) . filter (inRange bnds) . neighbours8
    startSand = [c | c <- indices land, land ! c, hasWater c]


growTree :: (Coord, Coord) -> UArray Coord Bool -> UArray Coord Bool -> UArray Coord Bool
growTree bnds land sand = grow bnds 2 allowed (const False) 4 startTrees
  where
    okNeighbours = filter (inRange bnds) . neighbours8
    allowed p = land ! p && not (any (sand !) (okNeighbours p))

    fitsPattern p = and $ (\c -> inRange bnds (p+c) && land ! (p+c)) <$> xs
      where
        xs = [(-1,-2), (0,-2), (1,-2)
             , (-2,-1), (-1,-1), (0,-1), (1,-1), (2,-1)
             , (-2,0), (-1,0), (1,0), (2,0)
             , (-2,1), (-1,1), (0,1), (1,1), (2,1)
             , (-1,2), (0,2), (1,2)]
    startTrees = [c | c <- indices land, land ! c, fitsPattern c]


day5 :: IO ()
day5 = do
  s1 <- getLines (5, 1)
  s2 <- getLines (5, 2)
  s3 <- getLines (5, 3)

  let ones1 = getOnes s1
      ones2 = getOnes s2
      ones3 = getOnes s3
      onesArray3 :: UArray Coord Bool
      onesArray1 = array ((0,0), max1- (1,1)) [(c, True) | c <- ones1]
      onesArray2 = array ((0,0), max2 - (1,1)) [(c, True) | c <- ones2]
      onesArray3 = array ((0,0), max3 - (1,1)) [(c, True) | c <- ones3]

      start1 = getStart s1
      start2 = getStart s2
      start3 = getStart s3
      
      --land1 = addLand inBounds1 start1 (S.fromList ones1)
      land1Array = growLand ((0,0), max1 - (1,1)) onesArray1 start1

      --land2 = addLand inBounds2 start2 (S.fromList ones2)
      --sand2 = addSand inBounds2 land2
      land2Array = growLand ((0,0), max2 - (1,1)) onesArray2 start2
      sand2Array = growSand ((0,0), max2 - (1,1)) land2Array

      land3Array = growLand ((0,0), max3 - (1,1)) onesArray3 start3
      sand3Array = growSand ((0,0), max3 - (1,1)) land3Array
      tree3Array = growTree ((0,0), max3 - (1,1)) (land3Array // [(c, False) | (c, True) <- assocs sand3Array]) sand3Array
  
  putStrLn $ "Quest5: part1: " ++ show (sizeArray land1Array)
  putStrLn $ "Quest5: part2: " ++ show (sizeArray sand2Array)
  putStrLn $ "Quest5: part3: " ++ show (sizeArray tree3Array)
  return ()


-- OLD CODE - a bit slow, but works. The new code above is a lot faster.

inBounds1, inBounds2, inBounds3 :: Coord -> Bool
inBounds1 (x,y) = x>=0 && x < fst max1 && y>=0 && y < snd max1
inBounds2 (x,y) = x>=0 && x < fst max2 && y>=0 && y < snd max2
inBounds3 (x,y) = x>=0 && x < fst max3 && y>=0 && y < snd max3



addLand :: (Coord -> Bool) -> Coord -> Set Coord -> Set Coord
addLand bounds start ones = go (S.singleton start) (S.singleton start)
  where
    go :: Set Coord -> Set Coord -> Set Coord
    go frontier land
      | S.null newFrontier = land
      | otherwise = go newFrontier (S.union newFrontier land)
      where
        newFrontier = S.filter (\c -> bounds c && c `S.notMember` land && isLand c) 
              $ S.unions $ S.map (S.fromList . neighbours8) frontier

        isLand :: Coord -> Bool
        isLand p = landCount >= 5
          where
            ns = filter bounds $ neighbours8 p
            landCount = length (filter (\c -> c `S.member` ones || c `S.member` land) ns)


addSand :: (Coord -> Bool) -> Set Coord -> Set Coord
addSand bounds startLand = go startSand startSand (startLand `S.difference` startSand)
  where
    hasWater :: Coord -> Bool
    hasWater p = not . null $ filter (\c -> c `S.notMember` startLand) $ neighbours8 p
    
    startSand = S.filter hasWater startLand

    go :: Set Coord -> Set Coord -> Set Coord -> Set Coord
    go frontier sand land
      | S.null newFrontier = sand
      | otherwise = go newFrontier (S.union newFrontier sand) (land `S.difference` newFrontier)
      where
        newFrontier = S.filter (\c -> bounds c && c `S.member` land && isSand c) 
              $ S.unions $ S.map (S.fromList . neighbours8) frontier

        isSand :: Coord -> Bool
        isSand p = sandCount >= 5
          where
            ns = filter bounds $ neighbours8 p
            sandCount = length (filter (`S.member` sand) ns)


addTrees :: (Coord -> Bool) -> Set Coord -> Set Coord -> Set Coord
addTrees bounds startLand sand = go 2 startTrees (startLand `S.difference` startTrees)
  where
    fitsPattern :: Coord -> Bool
    fitsPattern p = and $ (\c -> (p+c) `S.member` startLand) <$> xs
      where
        xs = [(-1,-2), (0,-2), (1,-2)
             , (-2,-1), (-1,-1), (0,-1), (1,-1), (2,-1)
             , (-2,0), (-1,0), (1,0), (2,0)
             , (-2,1), (-1,1), (0,1), (1,1), (2,1)
             , (-1,2), (0,2), (1,2)]
    
    startTrees = S.filter fitsPattern startLand

    go :: Int -> Set Coord -> Set Coord -> Set Coord
    go n trees land
      | n==0 = trees
      | S.null newTrees = trees
      | otherwise = go (n-1) (S.union newTrees trees) (land `S.difference` newTrees)
      where
        newTrees = S.filter isTree land

        isTree :: Coord -> Bool
        isTree p = treeCount >= 4 && sandCount == 0
          where
            ns = filter bounds $ neighbours8 p
            sandCount = length (filter (`S.member` sand) ns)
            treeCount = length (filter (`S.member` trees) ns)


