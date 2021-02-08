{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Types where

import Advent.Problem   (solution, ToString, Day(..))
import Data.Maybe       (fromJust)
import GHC.TypeLits

data Answer = forall n. KnownNat n => Answer {
            day :: Day n,
            partOne :: String,
            partTwo :: Maybe String
        }

class Result a where
    result :: a -> Maybe String

data Answers = Answers {
            when :: Integer,
            solutions :: [Answer]
        }

answer :: (Result a, Result b, KnownNat n) => Day n -> a -> b -> Answer
answer day partOne partTwo = Answer day (fromJust $ result partOne) (result partTwo)

-- This is an ugly and unsound type hack to help type inference,
-- but it works since we only want to use Nothing and never Just
instance (ToString a, a ~ Integer) => Result (Maybe a) where
    result = fmap solution

instance (Num a, a ~ Integer) => Result a where
    result = Just . solution
-- end of hack

instance Result String where
    result = Just . solution

