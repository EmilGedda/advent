module Solutions.Y2020.D24 where

import           Advent.Problem
import           Data.List      (sort, group)
import qualified Data.HashSet   as H

day24 :: Day
day24 = day 24 (exhibit 0) (exhibit 100)

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

black :: H.HashSet (Int, Int) -> H.HashSet (Int, Int)
black s =
    let
        adjacent = H.unions . map (H.fromList . neighbours) $ H.toList s
    in adjacent

white :: H.HashSet (Int, Int) -> H.HashSet (Int, Int)
white s = undefined

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
