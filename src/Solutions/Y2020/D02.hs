{-# LANGUAGE OverloadedStrings #-}
module Solutions.Y2020.D02 (day02) where

import Prelude hiding                   (drop)
import Advent.Problem                   (Day, day, Parseable(..), fromRight)
import Data.Attoparsec.ByteString.Char8 (Parser, decimal, char, anyChar, many', parseOnly)

data Policy = Policy Int Int Char String

policy :: Parser Policy
policy = Policy
       <$> decimal
       <*> (char '-' *> decimal)
       <*> (char ' ' *> anyChar)
       <*> (": " *> many' anyChar)

instance Parseable Policy where
    parseInput = fromRight .  parseOnly policy


day02 :: Day 2
day02 = day (length . filter valid1) (length . filter valid2)

valid1 :: Policy -> Bool
valid1 (Policy lower upper char password) = lower <= count && count <= upper
    where count = length . filter (char ==) $ password

valid2 :: Policy -> Bool
valid2 (Policy lower upper char password) = equal lower /= equal upper
    where equal at = (password !! (at - 1)) == char
