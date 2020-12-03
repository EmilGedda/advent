{-# LANGUAGE OverloadedStrings #-}
module Advent.Solution.DayTwo (day2) where

import Prelude hiding                   (drop)
import Advent.Problem                   (Day, day, Parseable(..), fromRight)
import Data.Attoparsec.ByteString.Char8 (decimal, char, anyChar, many', parseOnly)

data Policy = Policy Int Int Char String

policy = Policy
       <$> decimal
       <*> (char '-' *> decimal)
       <*> (char ' ' *> anyChar)
       <*> (": " *> many' anyChar)

instance Parseable Policy where
    parseInput = fromRight .  parseOnly policy


day2 :: Day
day2 = day 2 (length . filter valid1) (length . filter valid2)

valid1 :: Policy -> Bool
valid1 (Policy lower upper char password) = lower <= count && count <= upper
    where count = length . filter (char ==) $ password

valid2 :: Policy -> Bool
valid2 (Policy lower upper char password) = equal lower /= equal upper
    where equal at = (password !! (at - 1)) == char
