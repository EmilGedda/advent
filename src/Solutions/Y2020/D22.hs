{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
module Solutions.Y2020.D22 (day22) where

import           Advent.Problem
import           Data.Hashable      (Hashable)
import           Data.List          (isPrefixOf)
import           Data.Sequence      (Seq(..))
import           Data.Foldable
import qualified Data.Sequence      as S
import qualified Data.HashSet       as Set

newtype Decks = Decks { getDecks :: (Seq Int, Seq Int) }
    deriving (Eq, Generic, NFData)

type Deck = Seq Int

instance Hashable Decks

instance Parseable Decks where
    parseString = Decks
                . parseString
                . unlines
                . filter (not . isPrefixOf "Player")
                . lines

day22 :: Day 22
day22 = day (play combat) (play recursiveCombat)

play :: (Seq Int -> Seq Int -> Deck) -> Decks -> Int
play game = winner . uncurry game . getDecks

winner :: Deck -> Int
winner = sum . zipWith (*) [1..] . toList . S.reverse

combat :: Deck -> Deck -> Deck
combat S.Empty p2 = p2
combat p1 S.Empty = p1
combat p1 p2 = topdeck combat p1 p2

topdeck :: Ord a => (Seq a -> Seq a -> b) -> Seq a -> Seq a -> b
topdeck f (x :<| xs) (y :<| ys)
    | x > y     = f (xs :|> x :|> y) ys
    | otherwise = f xs (ys :|> y :|> x)
topdeck f xs ys = f xs ys

recursiveCombat :: Deck -> Deck -> Deck
recursiveCombat p1 p2 = snd $ go Set.empty p1 p2
    where go _ S.Empty p2 = (False, p2)
          go _ p1 S.Empty = (True, p1)
          go history p1@(x :<| xs) p2@(y :<| ys)
            | game `Set.member` history = (True, p1)
            | x <= S.length xs && y <= S.length ys =
                if maximum p1Subdeck > maximum p2Subdeck
                || fst (go seen p1Subdeck p2Subdeck)
                    then go seen (xs :|> x :|> y) ys
                    else go seen xs (ys :|> y :|> x)
            | otherwise = topdeck (go seen) p1 p2
            where game = p1 <> ys
                  seen = Set.insert game history
                  p1Subdeck = S.take x xs
                  p2Subdeck = S.take y ys
