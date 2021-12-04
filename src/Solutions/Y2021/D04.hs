{-# LANGUAGE OverloadedStrings #-}
module Solutions.Y2021.D04 where

import Advent.Problem
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.List
import qualified Data.Set as S

type Board = [S.Set Int]

data Bingo = Bingo [Int] [[[Int]]] deriving (Generic, NFData)

bingo :: Parser Bingo
bingo =
    let number = decimal <|> (" " *> decimal)
     in Bingo <$> number `sepBy1` "," <* "\n\n"
              <*> number `sepBy1` " "
                         `sepBy1` "\n"
                         `sepBy1` "\n\n"

instance Parseable Bingo where
    parseInput = attoparse bingo


day04 :: Day 4
day04 = day (head . winners) (last . winners)

mkSets :: [[Int]] -> Board
mkSets board = map S.fromList board ++ map S.fromList (transpose board)

score :: Int -> Board -> Int
score card board = card * sum (S.toList =<< board) `div` 2

mark :: Int -> [Board] -> [Board]
mark card = map . map $ S.delete card

winners :: Bingo -> [Int]
winners (Bingo numbers boards) = go numbers $ map mkSets boards
    where go _ [] = []
          go (card:cards) boards =
              case partition (any S.null) $ mark card boards of
                (won, rest) -> map (score card) won ++ go cards rest
