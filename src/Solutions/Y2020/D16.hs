{-# LANGUAGE OverloadedStrings #-}
module Solutions.Y2020.D16 (day16) where

import Advent.Problem
import Data.Maybe                       (mapMaybe, isJust)
import Data.List                        (find, transpose, intersect, sortOn, (\\) , isPrefixOf, foldl')
import Data.Attoparsec.ByteString.Char8 (manyTill, parseOnly, decimal, sepBy, anyChar)

data Rule = Rule {
            name :: String,
            first :: (Int, Int),
            second :: (Int, Int)
        } deriving (Show, Generic, NFData)

type Ticket = CommaList Int

data Notes = Notes [Rule] Ticket [Ticket] deriving (Show, Generic, NFData)

rule = Rule
       <$> manyTill anyChar ": "
       <*> pair
       <*> (" or " *> pair)
    where pair = (,) <$> decimal <*> ("-" *> decimal)

notes = Notes
        <$> rule `sepBy` "\n"
        <*> ("\n\nyour ticket:\n" *> commalist decimal)
        <*> ("\n\nnearby tickets:\n" *> commalist decimal `sepBy` "\n")

instance Parseable Notes where
    parseInput = fromRight . parseOnly notes


day16 :: Day 16
day16 = day partOne' partTwo'

partOne' :: Notes -> Int
partOne' (Notes rules _ nearby)
  = sum $ mapMaybe (find (not . validate rules) . getList) nearby

partTwo' :: Notes -> Int
partTwo' (Notes rules ticket nearby)
  = product . map fst
  . filter (isPrefixOf "departure" . snd)
  . foldl' (\acc (v, x) -> (v, head $ x \\ map snd acc):acc) []
  . sortOn (length . snd)
  . zip (getList ticket)
  . map (foldr1 intersect . map (flip mapMaybe rules . valid))
  . transpose
  $ map (filter (validate rules) . getList) nearby

validate :: Foldable t => t Rule -> Int -> Bool
validate rules = flip any rules . (isJust .) . valid

valid :: Int -> Rule -> Maybe String
valid x (Rule rule (a,b) (c,d))
    | between a b x || between c d x = Just rule
    | otherwise = Nothing
