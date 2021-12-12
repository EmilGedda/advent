{-# LANGUAGE LambdaCase #-}
module Solutions.Y2021.D10 (day10) where

import Advent.Problem
import Data.Either
import Data.List

day10 :: Day 10
day10 = day (syntaxError . map parse) (autocompete . map parse)

syntaxError :: [Either a Char] -> Int
syntaxError = sum . map corruptScore . rights

autocompete :: [Either String a] -> Int
autocompete lines = sort score !! (length score `div` 2)
    where score = map (foldl' (\acc n -> acc * 5 + completionScore n) 0)
                $ lefts lines

corruptScore :: Char -> Int
corruptScore = \case
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137

completionScore :: Char -> Int
completionScore = \case
    '(' -> 1
    '[' -> 2
    '{' -> 3
    '<' -> 4

parse :: String -> Either String Char
parse = go []
    where
        go leftover [] = Left leftover
        go ('(':stack) (')':rest) = go stack rest
        go ('[':stack) (']':rest) = go stack rest
        go ('{':stack) ('}':rest) = go stack rest
        go ('<':stack) ('>':rest) = go stack rest
        go stack (next:rest)
            | next `elem` "([{<" = go (next:stack) rest
            | otherwise = Right next
