{-# LANGUAGE DeriveGeneric #-}
module Solutions.Y2020.D22 (day22) where

import           Advent.Problem
import           GHC.Generics       (Generic)
import           Data.Hashable      (Hashable)
import           Data.List          (isPrefixOf)
import qualified Data.HashSet       as S

data Decks = Decks [Int] [Int] deriving (Eq, Generic)

instance Hashable Decks

instance Parseable Decks where
    parseString = uncurry Decks
                . parseString
                . unlines
                . filter (not . isPrefixOf "Player")
                . lines

day22 :: Day
day22 = day 22 (play . combat) (play . recursiveCombat)

play :: Decks -> Int
play (Decks p1 p2) = sum . zipWith (*) [1..] . reverse $ p1 ++ p2

combat :: Decks -> Decks
combat g@(Decks [] _) = g
combat g@(Decks _ []) = g
combat g = combat . topdeck $ g

topdeck (Decks (x:xs) (y:ys))
    | x > y     = Decks (xs ++ [x, y]) ys
    | otherwise = Decks xs (ys ++ [y, x])

recursiveCombat :: Decks -> Decks
recursiveCombat = go S.empty
    where go _ game@(Decks [] _) = game
          go _ game@(Decks _ []) = game
          go history game@(Decks p1@(x:xs) (y:ys))
            | game `S.member` history = Decks p1 []
            | x > length xs || y > length ys = go (S.insert game history) $ topdeck game
            | otherwise = case go S.empty $ Decks (take x xs) (take y ys) of
                            Decks _ [] -> go history $ Decks (xs ++ [x, y]) ys
                            Decks [] _ -> go history $ Decks xs (ys ++ [y, x])
