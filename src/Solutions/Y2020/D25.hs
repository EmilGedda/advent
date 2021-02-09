{-# LANGUAGE BangPatterns #-}
module Solutions.Y2020.D25 (day25) where

import Advent.Problem
import Data.Bits

day25 :: Day 25
day25 = day encKey notSolved

encKey :: [Int] -> Int
encKey [card, door] = modexp door (loopSize card)

loopSize :: Integral a => a -> Int
loopSize c = length . takeWhile (/=c) $ iterate (transform 7) 1

modexp :: Int -> Int -> Int
modexp b e = go e 1 b
    where go 0 res _ = res
          go exp res base =
              let res' | odd exp = transform res base
                       | otherwise = res
              in go (exp `shiftR` 1) res' (transform base base)

transform :: Integral a => a -> a -> a
transform b e = b * e `mod` 20201227
