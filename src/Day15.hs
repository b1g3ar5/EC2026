module Day15(day15) where

import Utils
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.PQueue.Prio.Min as PQ
import qualified GHC.Float as M
import GHC.Base (VecElem(Int8ElemRep, Int16ElemRep))


parse :: [String] -> Map Char (Map Char Int)
parse ls = undefined
  where
    cs :: [(Coord, Char)]
    cs = parseGridWith id ls

    wid = length $ ls!!0
    hgt = length ls

    mp = M.map (S.fromList) $ foldl (\m (p,c) -> M.insertWith (++) c [p] m) M.empty cs
    sulphur = mp M.! '@'
    start = fst $ fromJust $ S.minView $ mp M.! 'S'
    finish = fst $ fromJust $ S.minView $ mp M.! 'E'
    track = S.unions [(mp M.! '.'), sulphur, S.fromList [start, finish]]
    nodes = S.insert start $ S.insert finish $ S.filter isNode track
    numNodes = S.size nodes

    isNode c = length (filter (`S.member` track) $ neighbours4 c) > 2

    findNode pos n seen
      | nxt `S.member` nodes = (nxt, n+1)
      | otherwise = findNode nxt (n+1) (pos:seen)
      where
        nxt = head $ filter (\x -> x `notElem` seen && x `S.member` track) $ neighbours4 pos

    makeNet :: Coord -> Map Coord (Map Coord Int) -> Map Coord (Map Coord Int)
    makeNet pos mp
      | M.size mp == numNodes = mp
      | otherwise = undefined
        where



day15 :: IO ()
day15 = do
  s1 <- getLines (15, 1)
  --s2 <- getLines (15, 2)
  --s3 <- getLines (15, 3)
  --let s3 = test2
  let mp1 = parse s1

  putStrLn $ "Quest15: part1: " ++ show (223)
  -- 2020 loops of the loop get the other 6 on the way out
  -- 9 + 8*2019 + 108 = 16152+117=16269
  putStrLn $ "Quest15: part2: " ++ show (16269)
  -- best loop is 94 steps for 58 sulphur
  -- 202620262026 / 58 = 3493452792
  -- So do 3493452791 loops gives 202620262026 - 58*3493452791 = 90 sulphur on the way out
  -- So what's the best route S->E collecting 90?
  putStrLn $ "Quest15: part3: " ++ show (328384562637)
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