module Day14(day14) where

import Utils
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.PQueue.Prio.Min as PQ
import qualified GHC.Float as M
import GHC.Base (VecElem(Int8ElemRep, Int16ElemRep))


parse :: [String] -> Map Char (Set Coord)
parse ls = M.insert ' ' boundary $ M.map (S.fromList) $ foldl (\m (p,c) -> M.insertWith (++) c [p] m) M.empty cs
  where
    cs :: [(Coord, Char)]
    cs = parseGridWith id ls

    wid = length $ ls!!0
    hgt = length ls

    boundary = S.fromList $ [(-1,-1), (-1,hgt), (wid, -1), (wid, hgt)] ++ concat [[(-1,y), (wid,y), (x,-1), (x,hgt)]| x <- [0..wid-1], y <- [0..hgt-1]]


boundary :: Set Coord -> Int
boundary cs = sum [ length $ filter (`S.notMember` cs) (neighbours4 c) 
                  | c <- S.toList cs ]


type EdgeMap  = Map Char (Map Char Int)

mkEdgeMap :: Map Char (Set Coord) -> Map (Char, Char) Int
mkEdgeMap smp = M.fromListWith (+)
  [ ((s, t), 1)
  | (s, cs) <- M.toList smp
  , c <- S.toList cs
  , n <- neighbours4 c
  , Just t <- [M.lookup n owner]
  , t /= s
  ]
  where
    owner :: Map Coord Char
    owner = M.fromList [ (c, s) | (s, cs) <- M.toList smp, c <- S.toList cs ]


mkMap2 :: Map (Char, Char) Int -> EdgeMap
mkMap2 mp = M.fromList $ (\g -> (fst $ head g, M.fromList $ snd <$> g)) 
                        <$> groupBy (\a b -> fst a == fst b)  
                                    (sort $ (\((a,b), c) -> (a, (b,c))) <$> M.toList mp)


minBinding2 :: EdgeMap -> Int
minBinding2 emp = go (S.singleton ' ') (emp M.! ' ') 0
  where
    n = M.size emp
    go fixed frontier dist
      | S.size fixed == n = dist
      | otherwise = go newFixed newFrontier (dist+i)
        where
          --nxts = M.unionsWith min $ S.toList $ S.map (\v -> M.filterWithKey (\k _ -> k `S.notMember` fixed) $ emp `mm` v) fixed
          --(nxtPos,i) = minimumBy (compare `on` snd) $ M.toList nxts
          (v, i)  = minimumBy (compare `on` snd) (M.toList frontier)
          newFixed  = S.insert v fixed
          newFrontier  = M.delete v $ M.unionWith min frontier
                    (M.filterWithKey (\k _ -> k `S.notMember` newFixed) (emp M.! v))


minBinding3 :: EdgeMap -> (Char, Char) -> Int
minBinding3 emp (a,b) = minBinding2 $ M.mapWithKey charge emp
  where
    charge s = M.mapWithKey (\t n -> if s `elem` [a,b] || t `elem` [a,b] then 0 else n)
    


toEdge :: Char -> Map (Char, Char) Int -> Map (Char, Char) Int
toEdge a inMp = M.mapWithKey (\(x,y) v -> if ((x==a) || (y==a))
                                           then 0
                                           else v
                              ) inMp


day14 :: IO ()
day14 = do
  s1 <- getLines (14, 1)
  s2 <- getLines (14, 2)
  s3 <- getLines (14, 3)
  --let s3 = test2
  let mp1 = parse s1
      mp2 = parse s2
      mp3 = parse s3
      emp21 = mkEdgeMap mp2
      emp22 = mkMap2 emp21
      
      emp3 = mkMap2 $ mkEdgeMap mp3
      pairs = [(a,b) | a <- M.keys mp3, b <- M.keys mp3, a /= ' ', b /= ' ', a /= b]
      pbinding = (minBinding3 emp3) <$> pairs

  putStrLn $ "Quest14: part1: " ++ show (sum (M.elems (mkEdgeMap mp1)) `div` 2)
  putStrLn $ "Quest14: part2: " ++ show (minBinding2 emp22)
  putStrLn $ "Quest14: part3: " ++ show (minimum pbinding)
  return ()


test = ["AAAEEEFFFF"
  , "AADDDDDGFF"
  , "BACDDIGGFF"
  , "BABBIIIGGH"
  , "BBBBIIIIHH"]

test2 = ["IIIIIILOHHHHHHHHGGGGGQSRRRRKKK"
  , "IIIIIILOOHHHHHHGGXGGGQSRRRRKKK"
  , "PPPPILLLOOOODDHBGGGGGQSSRRRKKK"
  , "PPZPLLLLFFOODDBBBBGGQQQSSRKKKK"
  , "PPPPLLLLFFFONDDDBBMGGQQSSKKJJJ"
  , "PPPCCCCCCFFONNDDMMMGQQQSSSKJJJ"
  , "PCCCCCCCCFFNNDDDDMAAQQQSSSKKJJ"
  , "PPCCECCCFFNNNNDADMAATQTTSSSJJJ"
  , "PPEEEEECCFFNNAAAMMATTTTTSTSSSJ"
  , "PPEEEEEEEFFNNNAAAAAATTTTTTSSJJ"]