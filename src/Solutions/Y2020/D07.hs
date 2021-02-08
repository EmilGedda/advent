{-# LANGUAGE OverloadedStrings #-}

module Solutions.Y2020.D07 (day07) where

import           Advent.Problem                           (Parseable, Day, parseInput,
                                                                day, fromRight, count)
import           Control.Applicative                      ((<|>))
import           Data.ByteString                          (ByteString)
import           Data.Functor                             (($>))
import           Data.Attoparsec.ByteString.Char8 hiding  (count)
import qualified Data.Map.Strict                  as M

data Bag = Bag ByteString ByteString deriving (Eq, Ord)

data Rule = Rule Bag [(Int, Bag)]

word :: Parser ByteString
word = takeTill (==' ')

bag :: Parser Bag
bag = Bag <$> word <*> (" " *> word <* " bag" <* option "" "s")

constraint :: Parser (Int, Bag)
constraint = (,) <$> decimal <*> (space *> bag)

rule :: Parser Rule
rule = Rule
     <$> bag
     <*> (" contain "
      *> ("no other bags" $> []
     <|> constraint `sepBy` ", "))
      <* "."

instance Parseable Rule where
    parseInput = fromRight . parseOnly rule

day07 :: Day 7
day07 = day partOne partTwo

own :: Bag
own = Bag "shiny" "gold"

partOne :: [Rule] -> Int
partOne rules = bags (count (/=0)) (luggage rules own) rules

partTwo :: [Rule] -> Int
partTwo rules = bags sum (flip (luggage rules) own) rules

fits :: M.Map Bag [(Int, Bag)] -> Bag -> Bag -> Int
fits bags want has
    | want == has = 1
    | null (bags M.! has) = 0
    | otherwise = sum . map next $ bags M.! has
    where next (n, bag) = n * fits bags want bag

bags :: Num b => ([a] -> b) -> (Bag -> a) -> [Rule] -> b
bags collect f = flip (-) 1 . collect . map (f . fst . fromRule)

luggage :: [Rule] -> Bag -> Bag -> Int
luggage = fits . M.fromList . map fromRule

fromRule :: Rule -> (Bag, [(Int, Bag)])
fromRule (Rule b c) = (b,c)
