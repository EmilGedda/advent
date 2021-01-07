{-# LANGUAGE OverloadedStrings #-}
module Solutions.Y2020.D18 (day18) where

import           Advent.Problem                   (day, Day, attoparse)
import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as B

day18 :: Day
day18 = day 18 (eval grammar1) (eval grammar2)

eval :: Parser Int -> [B.ByteString] -> Int
eval parser = sum . map (attoparse parser . B.reverse)

grammar1 :: Parser Int
grammar1 = binop (+) " + " <|> binop (*) " * " <|> term grammar1
    where binop e token = e <$> term grammar1 <* token <*> grammar1

grammar2 :: Parser Int
grammar2 = (*) <$> add <* " * " <*> grammar2 <|> add
    where add = (+) <$> term grammar2 <* " + " <*> add <|> term grammar2

term :: Parser Int -> Parser Int
term p = ")" *> p <* "(" <|> decimal
