module Solutions.Y2020.D24 where

import           Advent.Problem
import           Data.List      (sort, group)
import qualified Data.HashSet   as H

day24 :: Day 24
day24 = day (exhibit 0) (exhibit 100)

exhibit :: Int -> [String] -> Int
exhibit n = H.size
          . (!! n)
          . iterate flipTiles
          . tiles

tiles :: [String] -> H.HashSet (Int, Int)
tiles = H.fromList
      . map head
      . filter (odd . length)
      . group
      . sort
      . map (foldr add (0, 0) . steps)

flipTiles :: H.HashSet (Int, Int) -> H.HashSet (Int, Int)
flipTiles s = black s `H.difference` white s

friends :: H.HashSet (Int, Int) -> (Int, Int) -> Int
friends s = length . filter (`H.member` s) . neighbours

black :: H.HashSet (Int, Int) -> H.HashSet (Int, Int)
black s = H.union s . H.fromList $ filter shouldSwap adjacent
    where shouldSwap = (2==) . friends s
          adjacent   = filter (not . flip H.member s) $ neighbours =<< H.toList s

white :: H.HashSet (Int, Int) -> H.HashSet (Int, Int)
white s = H.filter ((\x -> x == 0 || x > 2) . friends s) s

add :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
add (a, b) (c, d) = (a + c, b + d)

steps :: String -> [(Int, Int)]
steps "" = []
steps ('n':'w':rest) = (-1,  1):steps rest
steps ('n':'e':rest) = ( 1,  1):steps rest
steps ('s':'e':rest) = ( 1, -1):steps rest
steps ('s':'w':rest) = (-1, -1):steps rest
steps     ('e':rest) = ( 2,  0):steps rest
steps     ('w':rest) = (-2,  0):steps rest

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours p = map (add p) [(-1, 1), (1, 1), (1, -1), (-1, -1), (2, 0), (-2, 0)]
