{-# LANGUAGE TupleSections #-}
module Solutions.Y2020.D23 (day23) where

import           Advent.Problem
import           Control.Monad
import           Control.Monad.ST
import           Data.List                   (sortOn)
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

day23 :: Day
day23 = day 23 (fromDigits . take 8 . crab 9 100)
               (product . take 2 . crab 1000000 10000000)

crab :: Int -> Int -> Int -> [Int]
crab l n = toList
       . (\(start, v) -> U.modify (moveCups n start) v)
       . createVector
       . flip mappend [10..l]
       . toDigits

createVector :: [Int] -> (Int, U.Vector Int)
createVector xs = (head xs,)
                . U.fromList
                . map snd
                . sortOn fst
                . zip xs
                . tail
                $ cycle xs

toList :: U.Vector Int -> [Int]
toList v = go 1
    where go at = let x = v U.! (at - 1) in x:go x

moveCups :: Int -> Int -> UM.MVector s Int -> ST s ()
moveCups n start v =

    let
        len     = UM.length v
        read i  = UM.unsafeRead  v (i - 1)
        write i = UM.unsafeWrite v (i - 1)

        decrease n
            | n <= 1 = len
            | otherwise = n - 1

        destination n a b c
            | n == a || n == b || n == c
                = destination (decrease n) a b c
            | otherwise = n

    in void
     . fold [0..n - 1] start . flip . const $ \current -> do
          a    <- read current
          b    <- read a
          c    <- read b
          next <- read c
          write current next

          let dest = destination (decrease current) a b c
          after <- read dest
          write dest a
          write c after

          return next
