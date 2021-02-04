module Solutions.Y2020.D23 where

import           Advent.Problem
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

day23 :: Day
day23 = day 23 (crab 100) notSolved

reorder :: Integral a => [a] -> [a]
reorder = take 8 . drop 1 . dropWhile (/=1) . cycle

crab :: Int -> Int -> Int
crab n = fromDigits
       . reorder
       . U.toList
       . U.modify (moveCups n)
       . U.fromList
       . toDigits

moveCups :: Int -> UM.MVector s Int -> ST s ()
moveCups n v =
    let
        write i = UM.unsafeWrite v (i `mod` 9)
        read  i = UM.unsafeRead  v (i `mod` 9)
        readL i = mapM read [i..i + 9]
        writeL i l = zipWithM_ write [i..] l

        bound n
            | n < 1 = 9
            | otherwise = n

        destination n _ [] = n
        destination n l (x:xs)
            | n == x = destination (bound $ n - 1) l l
            | otherwise = destination n l xs

    in forM_ [0..n - 1] $ \i -> do
        current <- read i
        three <- take 3 <$> readL (i + 1)
        let dest = destination (bound $ current - 1) three three
        coming <- takeWhile (/= dest) <$> readL (i + 4)
        writeL (i + 1) $ coming ++ dest:three
