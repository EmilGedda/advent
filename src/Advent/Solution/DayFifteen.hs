module Advent.Solution.DayFifteen where

import           Advent.Problem                 (Day, notSolved, day, Parseable(..), parseInput)
import           Data.ByteString.Char8          (split)
import           Data.Bool                      (bool)
import           Control.Monad                  (foldM)
import           Control.Monad.ST               (runST)
import qualified Data.Vector.Unboxed.Mutable    as UM
import qualified Data.IntMap.Strict             as M

newtype Numbers = Numbers { getNumbers :: [Int] }

instance Parseable Numbers where
    parseInput = Numbers . map parseInput . split ','

day15 :: Day
day15 = day 15 (speak 2020) (speak 30000000)

speak :: Int -> Numbers -> Int
speak n (Numbers xs) = runST $ do
    let fold v s f = foldM f s v

    mem <- UM.new n
    mapM_ (uncurry (UM.write mem)) $ zip xs [1..]

    fold [length xs + 1..n] (last xs) $ \prev i -> do
        v <- UM.read mem prev
        UM.write mem prev (i - 1)
        return $ bool 0 (i - 1 - v) (v > 0)
