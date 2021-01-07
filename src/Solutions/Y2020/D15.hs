module Solutions.Y2020.D15 where

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

    fold [length xs..n - 1] (last xs) $ \prev i -> do
        v <- UM.read mem prev
        UM.write mem prev i
        return $ bool 0 (i - v) (v > 0)
