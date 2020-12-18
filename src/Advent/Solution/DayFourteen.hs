{-# LANGUAGE OverloadedStrings #-}
module Advent.Solution.DayFourteen (day14) where

import Advent.Problem                   (Day, notSolved, notSolved, day, both, fromBits, fromRight, Parseable(..))
import Control.Arrow                    ((&&&))
import Control.Applicative              ((<|>))
import Data.List                        (foldl')
import Data.Bits                        ((.|.), (.&.), complement, Bits)
import Data.Attoparsec.ByteString.Char8
import Text.Printf                      (printf)
import Numeric                          (showIntAtBase)
import qualified Data.Map               as M

data Instruction = Mask String | Store Int Integer

maskP  = Mask <$> ("mask = " *> many1 anyChar)
storeP = Store <$> ("mem[" *> decimal) <*> ("] = " *> decimal)

instance Parseable Instruction where
    parseInput = fromRight . parseOnly (maskP <|> storeP)

data CPU = CPU {
            mask :: String,
            mem :: M.Map Int Integer
        }

type Part = String -> Int -> Integer -> [(Int, Integer)]

day14 :: Day
day14 = day 14 (run partOne) (run partTwo)

partOne :: Part
partOne mask to value = return (to, set (fromstr mask) value)
    where set (zeros, ones) v = ones .|. v .&. complement zeros

partTwo :: Part
partTwo mask to value =
    let addresses = go "" $ zipWith merge mask bit
        bit = printf "%036s" $ showIntAtBase 2 ("01"!!) to ""
        merge '0' y = y
        merge '1' _ = '1'
        merge 'X' _ = 'X'
        go prev [] = return $ reverse prev
        go prev ('X':mask) = go prev ('0':mask) ++ go prev ('1':mask)
        go prev (c:mask) = go (c:prev) mask
    in map (\x -> (fromBits $ map (fromEnum . (=='1')) x, value)) addresses

run :: Part -> [Instruction] -> Integer
run r = sum . M.elems . mem . foldl run (CPU "" M.empty)
    where run c (Mask s) = c{ mask = s }
          run c@(CPU mask m) s@(Store to value)
                = c{ mem = foldl' (flip (uncurry M.insert)) m (r mask to value)  }

fromstr :: String -> (Integer, Integer)
fromstr = both fromBits . (map (bit '0') &&& map (bit '1'))
    where bit x v = fromIntegral . fromEnum $ v == x
