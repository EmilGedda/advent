module Solutions.Y2020.D25 (day25) where

import Advent.Problem
import Data.Bits
import Data.Maybe (catMaybes)
import qualified Data.IntMap as M

day25 :: Day 25
day25 = day diffieHellman notSolved

diffieHellman :: [Int] -> Int
diffieHellman [card, door] = modExp door (discreteLog card)

discreteLog :: Int -> Int
discreteLog = head . catMaybes . zipWith calc [0..] . iterate (transform factor)
    where table    = M.fromList . flip zip [0..modulo] $ iterate (transform 7) 1
          calc i e = (modulo * i +) <$> table M.!? e
          factor   = modExp 7 $ 20201227 - modulo - 1
          modulo   = ceiling  $ sqrt 20201227

modExp :: Int -> Int -> Int
modExp b e = go e 1 b
    where go 0   res _    = res
          go exp res base =
              let res' | odd exp   = transform res base
                       | otherwise = res
              in go (exp `shiftR` 1) res' (transform base base)

transform :: Int -> Int -> Int
transform b e = b * e `rem` 20201227
