{-# LANGUAGE OverloadedStrings #-}
module Solutions.Y2020.D19 where

import           Advent.Problem
import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8 hiding (count)
import           Data.Functor                     (($>))
import           Data.List                        (foldl1')
import           Data.Either                      (isRight)
import qualified Data.ByteString.Char8            as B
import qualified Data.Map                         as M

data Condition
    = Lit Char
    | If Int
    | And [Condition]
    | Or Condition Condition
    deriving Show

newtype Rule = Rule { fromRule :: (Int, Condition) } deriving Show

condition :: Parser Condition
condition = Lit <$> (char '"' *> letter_ascii <* char '"')
        <|> Or <$> or <* " | " <*> or
        <|> and
        <|> one
    where and = And <$> one `sepBy` " "
          or  = and <|> one
          one = If <$> decimal

rule :: Parser Rule
rule = curry Rule <$> decimal <* ": " <*> condition

instance Parseable Rule where
    parseInput = attoparse rule

day19 :: Day
day19 = day 19 (uncurry (valid id)) (uncurry (valid fix))

valid :: (M.Map Int Condition -> M.Map Int Condition)
      -> [Rule] -> [B.ByteString] -> Int
valid f = count . (isRight .) . parseOnly . parser . f . M.fromList . map fromRule

parser :: M.Map Int Condition -> Parser ()
parser m = go (If 0) *> endOfInput
    where go (Lit c)  = char c $> ()
          go (If a)   = go (m M.! a)
          go (And xs) = mapM_ go xs
          go (Or a b) = go a <|> go b

fix :: M.Map Int Condition -> M.Map Int Condition
fix = M.insert 8  (Or (If 42)
                      (And [If 42, If 8]))
    . M.insert 11 (Or (And [If 42, If 31])
                      (And [If 42, If 11, If 31]))
