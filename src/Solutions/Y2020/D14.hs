{-# LANGUAGE OverloadedStrings #-}
module Solutions.Y2020.D14 (day14) where

import Advent.Problem
import Control.Applicative              ((<|>))
import Data.Bits                        (setBit, clearBit)
import Data.Attoparsec.ByteString.Char8
import Data.IntMap                      (IntMap)
import qualified Data.IntMap            as IntMap

data Bit = B1 | B0 | BX deriving (Generic, NFData)
data Instruction = Mask [Bit] | Store Int Int deriving (Generic, NFData)

bitP :: Parser Bit
bitP = (B1 <$ char '1')
   <|> (B0 <$ char '0')
   <|> (BX <$ char 'X')

maskP :: Parser Instruction
maskP  = Mask <$> ("mask = " *> many1 bitP)


storeP :: Parser Instruction
storeP = Store <$> ("mem[" *> decimal) <*> ("] = " *> decimal)

instance Parseable Instruction where
    parseInput = fromRight . parseOnly (maskP <|> storeP)

day14 :: Day 14
day14 = day (partOne' [] mempty) (partTwo' [] mempty)

partOne' :: [Bit] -> IntMap Int -> [Instruction] -> Int
partOne' _ mem [] = sum mem
partOne' _ mem (Mask mask:xs) = partOne' mask mem xs
partOne' mask mem (Store addr value:xs) = partOne' mask  memory xs
    where
        memory = IntMap.insert addr (go value 35 mask) mem

        go value bit (B1:bits) = go (setBit value bit) (bit - 1) bits
        go value bit (B0:bits) = go (clearBit value bit) (bit - 1) bits
        go value bit (BX:bits) = go value (bit - 1) bits
        go value _ [] = value


partTwo' :: [Bit] -> IntMap Int -> [Instruction] -> Int
partTwo' _ mem [] = sum mem
partTwo' _ mem (Mask mask:xs) = partTwo' mask mem xs
partTwo' mask mem (Store addr value:xs) = partTwo' mask memory xs
    where
        memory = foldr (`IntMap.insert` value) mem
               $ go addr 35 mask

        go value bit (B1:bits) = go (setBit value bit) (bit - 1) bits
        go value bit (B0:bits) = go value (bit - 1) bits
        go value bit (BX:bits) = do
            addr <- go (setBit value bit) (bit - 1) bits
            [addr, clearBit addr bit]
        go value _ [] = [value]
