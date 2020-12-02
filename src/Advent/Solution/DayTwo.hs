{-# LANGUAGE OverloadedStrings #-}
module Advent.Solution.DayTwo where

import Prelude hiding                   (drop)
import Advent.Problem                   (Day, day, Parseable(..), fromRight)
import Control.Lens                     ((^?), element)
import Data.Attoparsec.ByteString.Char8

data Policy = Policy Int Int Char String

policy = Policy
       <$> decimal
       <*> (char '-' *> decimal)
       <*> (char ' ' *> anyChar)
       <*> (": " *> many' anyChar)

instance {-# OVERLAPPING #-} Parseable Policy where
    parseInput = fromRight .  parseOnly policy


day2 :: Day
day2 = day 2 (length . filter valid1) (length . filter valid2)

valid1 :: Policy -> Bool
valid1 (Policy lower upper char password) = lower <= occurs && occurs <= upper
    where occurs = length . filter (char ==) $ password

valid2 :: Policy -> Bool
valid2 (Policy lower upper char password) = correct lower /= correct upper
    where correct at = Just True == ((== char) <$> password ^? element (at - 1))
