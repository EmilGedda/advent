{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Advent.Solution.DaySixteen where

import Advent.Problem                   (Day, day, CommaList(..), fold, commalist, Parseable(..), fromRight, notSolved, between, CommaList, getList)
import Data.Maybe                       (catMaybes, listToMaybe, mapMaybe, isJust)
import Data.List                        (find, transpose, intersect, sortOn, (\\), isPrefixOf, concatMap, foldl')
import Data.Attoparsec.ByteString.Char8
import Debug.Trace

data Rule = Rule {
            name :: String,
            first :: (Int, Int),
            second :: (Int, Int)
        } deriving Show

rule = Rule
       <$> manyTill anyChar ": "
       <*> pair
       <*> (" or " *> pair)
    where pair = (,) <$> decimal <*> ("-" *> decimal)

type Ticket = CommaList Int

data Notes = Notes [Rule] Ticket [Ticket] deriving Show

notes = Notes
        <$> rule `sepBy` char '\n'
        <*> ("\n\nyour ticket:\n" *> commalist decimal)
        <*> ("\n\nnearby tickets:\n" *> commalist decimal `sepBy` "\n")

instance Parseable Notes where
    parseInput = fromRight . parseOnly notes


day16 :: Day
day16 = day 16 partOne partTwo

partOne :: Notes -> Int
partOne (Notes rules _ nearby) = sum $ mapMaybe (verify . getList) nearby
    where verify = find (not . validate rules)

partTwo :: Notes -> Int
partTwo (Notes rules ticket nearby)
  = product . map fst
  . filter (isPrefixOf "departure" . snd)
  . foldl' (\acc (v, x) -> (v, head $ x \\ map snd acc):acc) []
  . sortOn (length . snd)
  . zip (getList ticket)
  . map (foldr1 intersect
       . map (flip mapMaybe rules . valid)
       . filter (validate rules))
  . transpose
  $ map getList nearby

validate :: Foldable t => t Rule -> Int -> Bool
validate rules = flip any rules . (isJust .) . valid

valid :: Int -> Rule -> Maybe String
valid x (Rule rule (a,b) (c,d))
    | between a b x || between c d x = Just rule
    | otherwise = Nothing
