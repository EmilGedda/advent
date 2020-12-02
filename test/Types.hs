{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Advent.Problem   (solution, ToString)
import Data.Maybe       (fromJust)

day :: (Result a, Result b) => Integer -> a -> b -> Answer
day n partOne partTwo = Answer n (fromJust $ result partOne) (result partTwo)

data Answer = Answer {
            num :: Integer,
            partOne :: String,
            partTwo :: Maybe String
        } deriving Show

class Result a where
    result :: a -> Maybe String

-- This is an ugly and unsound type hack to help type inference,
-- but it works since we only want to use Nothing and never Just
instance (ToString a, a ~ Integer) => Result (Maybe a)
    where result = fmap solution

instance (Num a, a ~ Integer) => Result a where
    result = Just . solution

instance Result String where
    result = Just . solution
-- end of hack

