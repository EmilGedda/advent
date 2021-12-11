{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Advent.Problem (
        module Advent.Problem.Util,
        module Advent.Problem.Coord,
        module Advent.Problem.Types,
        module Control.DeepSeq,
        module GHC.Generics,
        module Data.Foldable,
        Day(..),
        Year(..),
        SomeYear(..),
        Years,
        Input(..),
        SomeDay(..),
        Days(..),
        day,
        dayNum,
        someDayNum,
        yearNum,
        toDayList,
        notSolved,
        fetchInput,
    ) where

import Advent
import Advent.API
import Advent.Problem.Util
import Advent.Problem.Coord
import Advent.Problem.Types

import Prelude hiding         (readFile, writeFile, lines)
import Data.Foldable          (foldl')
import Control.DeepSeq        (NFData)
import Control.Monad.Catch    (MonadCatch)
import Control.Monad.Except   (MonadError)
import Data.ByteString        (ByteString)
import System.FilePath        ((</>))
import Text.Printf            (printf)
import GHC.Generics           (Generic)
import GHC.TypeLits
import Data.Proxy

newtype Input = Input { fromInput :: ByteString } deriving Show

data Day n = forall a b c. (Parseable a, ToString b, ToString c, KnownNat n, n <= 25)
         => Day {
           partOne :: a -> b,
           partTwo :: a -> c
        }

day :: (Parseable a, ToString b, ToString c, KnownNat n, n <= 25)
        => (a -> b) -> (a -> c) -> Day n
day = Day

data Year n ns where
    Year :: (KnownNat n, 2015 <= n) => Days ns -> Year n ns

data Days ns where
    EmptyDays :: Days '[]
    PushDay :: KnownNat n => Day n -> Days ns -> Days (n ': ns)

type Years = [SomeYear]

data SomeDay = forall d. KnownNat d => WrapDay (Day d)

data SomeYear = forall y d. KnownNat y => WrapYear (Year y d)

dayNum :: forall n. KnownNat n => Day n -> Integer
dayNum _ = natVal (Proxy :: Proxy n)

toDayList :: Days ns -> [SomeDay]
toDayList EmptyDays = []
toDayList (PushDay d rest) = WrapDay d:toDayList rest

yearNum :: forall n ns. KnownNat n => Year n ns -> Integer
yearNum _ = natVal (Proxy :: Proxy n)

notSolved :: Parseable a => a -> String
notSolved = const "Not solved"

someDayNum :: SomeDay -> Integer
someDayNum (WrapDay d) = dayNum d

fetchInput :: (MonadHTTP m, MonadError String m, MonadCatch m, MonadFS m)
    => Integer -> Integer -> m Input
fetchInput year day = do
    dir <- fmap (</> show year </> printf "%02d" day) cacheDir
    createDir dir
    let cache = dir </> "input.txt"
    exists <- hasFile cache
    if exists
       then Input <$> readFile cache
       else do
           i <- input year day
           writeFile cache i
           return $ Input i
