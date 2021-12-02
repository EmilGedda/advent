module Advent.Problem.Coord where
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Coord = Coord !Int !Int deriving (Generic, NFData)

overCoord :: (Int -> Int) -> Coord -> Coord
overCoord f (Coord a b) = Coord (f a) (f b)

combineCoord :: (Int -> Int -> a) -> Coord -> a
combineCoord f (Coord a b) = f a b


instance Num Coord where
    (Coord a b) + (Coord a' b') = Coord (a + a') (b + b')
    (Coord a b) * (Coord a' b') = Coord (a * a') (b * b')
    abs = overCoord abs
    signum = overCoord signum
    negate = overCoord negate
    fromInteger = error "fromInteger on Coord"
