module Solutions.Y2020.D25 (day25) where

import Advent.Problem

day25 :: Day 25
day25 = day encKey notSolved

encKey :: [Int] -> Int
encKey [card, door] = modpow door (loopSize card)

loopSize :: Int -> Int
loopSize n = length
           . takeWhile (/=n)
           $ iterate (flip mod 20201227 . (*7)) 1

modpow :: Int -> Int -> Int
modpow b e = (!! e) $ iterate (flip mod 20201227 . (*b)) 1

