{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Advent.Problem where

data Input = Input String deriving Show
data Answer = Answer (Either Integer String) deriving Show

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

day :: ToAnswer a => Integer -> (Input -> a) -> Maybe (Input -> a) -> Day
day number partOne partTwo = Day number (answer . partOne) $ (answer .) <$> partTwo

toString :: Answer -> String
toString (Answer (Left number))  = show number
toString (Answer (Right string)) = string

fromInput (Input str) = str
