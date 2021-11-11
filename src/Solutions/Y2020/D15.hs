{-# LANGUAGE BangPatterns #-}
module Solutions.Y2020.D15 (day15) where

import           Advent.Problem                 (Day, day, CommaList(..), fold)
import           Data.Bool                      (bool)
import           Control.Monad.ST               (runST)
import           Data.Int                       (Int32)
import           Data.List                      (genericLength)
import qualified Data.Vector.Unboxed.Mutable    as UM

day15 :: Day 15
day15 = day (speak 2020) (speak 30000000)

speak :: Int32 -> CommaList Int -> Int32
speak n (CommaList xs) = runST $ do
    mem <- UM.new $ fromIntegral n
    mapM_ (uncurry (UM.write mem)) $ zip xs [1..]

    fold [genericLength xs..n - 1] (fromIntegral $ last xs) $ \prev i -> do
        let addr = fromIntegral prev
        v <- UM.read mem addr
        UM.write mem addr i
        return . fromIntegral $ bool 0 (i - v) (v > 0)
