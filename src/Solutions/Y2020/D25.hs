{-# LANGUAGE BangPatterns #-}
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
modpow b e = go e 1
    where f n = n * b `mod` 20201227
          go 0 x = x
          go n !x = go (n - 1) (f x)
