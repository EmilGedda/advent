{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Advent.Problem where

import Prelude hiding       (readFile, writeFile)
import Advent.API           (get, input)
import Control.Monad        (unless)
import Data.ByteString.Lazy (ByteString, readFile, writeFile, stripSuffix)
import Data.Maybe           (fromMaybe)
import Control.Monad.Except (ExceptT, lift)
import System.Directory     (XdgDirectory(XdgConfig), getXdgDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath      ((</>))
import Text.Printf          (printf)

newtype Input = Input ByteString deriving Show
newtype Answer = Answer (Either Integer String) deriving Show

type Solution = Input -> Answer

class ToAnswer a where
    answer :: a -> Answer

instance ToAnswer Integer where
    answer = Answer . Left

instance ToAnswer Int where
    answer = Answer . Left . toInteger

instance ToAnswer String where
    answer = Answer . Right

data Day = Day {
               number :: Integer,
               partOne :: Solution,
               partTwo :: Solution
            }

notSolved :: Input -> String
notSolved = const "Not solved"

day :: (ToAnswer a, ToAnswer b) => Integer -> (Input -> a) -> (Input -> b) -> Day
day number partOne partTwo = Day number (answer . partOne) (answer .  partTwo)

toString :: Answer -> String
toString = either show id . fromAnswer

fromInput (Input str) = fromMaybe str $ stripSuffix "\n" str
fromAnswer (Answer e) = e

fetchInput :: Integer -> Integer -> ExceptT String IO Input
fetchInput year day = do
    dir <- lift $ (</> show year </> printf "%02d" day) <$> getXdgDirectory XdgConfig "AdventOfCode"
    lift $ createDirectoryIfMissing True dir
    let cache = dir </> "input.txt"
    hasFile <- lift $ doesFileExist cache
    input <- if hasFile then lift (readFile cache) else get (input year day)
    unless hasFile (lift $ writeFile cache input)
    return $ Input input

