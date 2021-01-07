{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Advent.Problem   (solution, ToString, Day(..))
import Data.Maybe       (fromJust)

answer :: (Result a, Result b) => Day -> a -> b -> Answer
answer day partOne partTwo = Answer day (fromJust $ result partOne) (result partTwo)

data Answer = Answer {
            day :: Day,
            partOne :: String,
            partTwo :: Maybe String
        }

class Result a where
    result :: a -> Maybe String

data Answers = Answers {
            when :: Integer,
            solutions :: [Answer]
        }

-- This is an ugly and unsound type hack to help type inference,
-- but it works since we only want to use Nothing and never Just
instance (ToString a, a ~ Integer) => Result (Maybe a) where
    result = fmap solution

instance (Num a, a ~ Integer) => Result a where
    result = Just . solution
-- end of hack

instance Result String where
    result = Just . solution

