module Advent.Solution.DayFifteen where

import           Advent.Problem                 (Day, day, CommaList(..), fold)
import           Data.Bool                      (bool)
import           Control.Monad.ST               (runST)
import qualified Data.Vector.Unboxed.Mutable    as UM

day15 :: Day
day15 = day 15 (speak 2020) (speak 30000000)

speak :: Int -> CommaList Int -> Int
speak n (CommaList xs) = runST $ do
    mem <- UM.new n
    mapM_ (uncurry (UM.write mem)) $ zip xs [1..]

    fold [length xs + 1..n] (last xs) $ \prev i -> do
        v <- UM.read mem prev
        UM.write mem prev (i - 1)
        return $ bool 0 (i - 1 - v) (v > 0)
