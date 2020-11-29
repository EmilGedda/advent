{-# LANGUAGE FlexibleInstances #-}

module Advent.Problem where

import Data.ByteString.Lazy (ByteString)

newtype Input = Input ByteString deriving Show
newtype Answer = Answer (Either Integer String) deriving Show

type Solution = Input -> Answer

class ToAnswer a where
    answer :: a -> Answer

instance ToAnswer Integer where
    answer = Answer . Left

instance ToAnswer String where
    answer = Answer . Right

data Day = Day {
               number :: Integer,
               partOne :: Solution,
               partTwo :: Maybe Solution
            }

day :: (ToAnswer a, ToAnswer b) => Integer -> (Input -> a) -> Maybe (Input -> b) -> Day
day number partOne partTwo = Day number (answer . partOne) $ (answer .) <$> partTwo

toString :: Answer -> String
toString = either show id . fromAnswer

fromInput (Input str) = str
fromAnswer (Answer e) = e
