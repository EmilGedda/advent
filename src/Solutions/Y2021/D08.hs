{-# LANGUAGE OverloadedStrings #-}
module Solutions.Y2021.D08 (day08) where

import           Advent.Problem
import           Data.Attoparsec.ByteString.Char8 hiding (count, take)
import           Data.List
import           Control.Arrow
import qualified Data.Map.Strict                  as M

data Notes = Notes [String] [String]
    deriving (Generic, NFData, Show)

instance Parseable Notes where
    parseInput = attoparse $ Notes <$> words <* " | " <*> words
       where words = many1 letter_ascii `sepBy1'` space

day08 :: Day 8
day08 = day (solve $ count known) (solve fromDigits)

solve :: ([Int] -> Int) -> [Notes] -> Int
solve f = sum . map (f . output)

known :: Int -> Bool
known n = n `elem` [1,4,7,8]

segmentsToDigit :: M.Map String Int
segmentsToDigit = M.fromList
                $ zip [ "abcefg"
                      , "cf"
                      , "acdeg"
                      , "acdfg"
                      , "bcdf"
                      , "abdfg"
                      , "abdefg"
                      , "acf"
                      , "abcdefg"
                      , "abcdfg" ]
                      [0..]

charCount :: String -> M.Map Int String
charCount = M.fromListWith (<>) . map (length &&& take 1) . group . sort

output :: Notes -> [Int]
output (Notes input output) =
    let (one:_:four:rest) = sortOn length input
        segment = charCount $ concat input

        e:_ = segment M.! 4
        f:_ = segment M.! 9
        b:_ = segment M.! 6
        a:_ = segment M.! 8 \\ [c]
        c:_ = one           \\ [f]
        d:_ = four          \\ [b,c,f]
        g:_ = last rest     \\ [a,b,c,d,e,f]

        translate :: Char -> Char
        translate n = flip (M.!) n
                        $ M.fromList [ (a, 'a')
                                     , (b, 'b')
                                     , (c, 'c')
                                     , (d, 'd')
                                     , (e, 'e')
                                     , (f, 'f')
                                     , (g, 'g') ]

     in map ((M.!) segmentsToDigit . sort . map translate) output
