{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Advent.Problem where

import           Prelude hiding           (readFile, writeFile, lines)
import           Advent.API               (get, input)
import           Control.Arrow            ((***))
import           Control.Monad            (unless, foldM)
import           Control.Monad.Except     (ExceptT, lift)
import           Data.ByteString          (ByteString, readFile, writeFile, stripSuffix)
import           Data.ByteString.Char8    (lines, readInt, readInteger, unpack, pack, split)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Monoid              (Sum(..), getSum)
import           Data.Maybe               (fromMaybe, fromJust)
import           Data.List                (foldl1')
import           System.Directory         (XdgDirectory(XdgConfig), getXdgDirectory
                                          , doesFileExist, createDirectoryIfMissing)
import           System.FilePath          ((</>))
import           Text.Printf              (printf)
import qualified Data.Set                 as Set
import qualified Data.Vector              as V
import qualified Data.Vector.Unboxed      as UV

newtype CommaList a = CommaList { getList :: [a] }

class Parseable a where
    parseInput :: ByteString -> a
    parseInput = parseString . unpack

    parseString :: String -> a
    parseString = parseInput . pack

instance Parseable Double where
    parseString = read

instance Parseable Integer where
    parseInput = fst . fromJust . readInteger

instance Parseable Int where
    parseInput = fst . fromJust . readInt

instance Parseable a => Parseable [a] where
    parseInput = map parseInput . lines

instance (Parseable a, UV.Unbox a) => Parseable (UV.Vector a) where
    parseInput = UV.fromList . parseInput

instance Parseable a => Parseable (V.Vector a) where
    parseInput = V.fromList . parseInput

instance Parseable a => Parseable (CommaList a) where
    parseInput = CommaList . map parseInput . split ','

class Show a => ToString a where
    solution :: a -> String

instance Show a => ToString a where
    solution = show

instance {-# OVERLAPPING #-} ToString Char where
    solution = return

instance {-# OVERLAPPING #-} ToString String where
    solution = id


newtype Input = Input ByteString deriving Show

data Day = forall a b c. (Parseable a, ToString b, ToString c)
         => Day {
           number :: Integer,
           partOne :: a -> b,
           partTwo :: a -> c
        }

day :: (Parseable a, ToString b, ToString c)
        => Integer -> (a -> b) -> (a -> c) -> Day
day = Day

-- TODO: move to Util
every :: Int -> [a] -> [a]
every n = map head . takeWhile (not . null) . iterate (drop n)

count :: (Foldable t, Enum b) => (a -> b) -> t a -> Int
count f = getSum . foldMap (Sum . fromEnum . f)

fromRight (Right r) = r
fromBool f x | f x = Just x
             | otherwise = Nothing

between :: Ord a => a -> a -> a -> Bool
between a b x = a <= x && x <= b

both f = f *** f

sortNub :: (Ord a) => [a] -> [a]
sortNub = Set.toList . Set.fromList

same :: (Eq a) => [a] -> Bool
same xs = all (== head xs) (tail xs)

fromBits :: Num a => [a] -> a
fromBits = foldl1' ((+) . (2*))

fold :: (Foldable t, Monad m) => t a -> b -> (b -> a -> m b) -> m b
fold v s f = foldM f s v

notSolved :: Parseable a => a -> String
notSolved = const "Not solved"

fromInput :: Input -> ByteString
fromInput (Input str) = fromMaybe str $ stripSuffix "\n" str

fetchInput :: Integer -> Integer -> ExceptT String IO Input
fetchInput year day = do
    dir <- lift $ (</> show year </> printf "%02d" day) <$> getXdgDirectory XdgConfig "AdventOfCode"
    lift $ createDirectoryIfMissing True dir
    let cache = dir </> "input.txt"
    hasFile <- lift $ doesFileExist cache
    input <- if hasFile then lift (readFile cache) else toStrict <$> get (input year day)
    unless hasFile (lift $ writeFile cache input)
    return $ Input input

