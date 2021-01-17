{-# LANGUAGE OverloadedStrings #-}
module Solutions.Y2020.D19 (day19) where

import           Advent.Problem
import           Control.Applicative              ((<|>))
import           Control.Monad                    (guard)
import           Data.Attoparsec.ByteString.Char8 hiding (count)
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
day19 = day 19 (uncurry . valid . parser $ If 0) (uncurry . valid $ recursiveParser . fix)

valid :: (M.Map Int Condition -> Parser b) -> [Rule] -> [B.ByteString] -> Int
valid p = count . (isRight .) . parseOnly . (<* endOfInput) . p . M.fromList . map fromRule

parser :: Condition -> M.Map Int Condition -> Parser String
parser start m = go start
    where go (Lit c)  = return <$> char c
          go (If a)   = go (m M.! a)
          go (And xs) = concat <$> mapM go xs
          go (Or a b) = go a <|> go b

recursiveParser :: M.Map Int Condition -> Parser ()
recursiveParser m = do
  r42 <- many1 $ parser (If 42) m
  r31 <- many1 $ parser (If 31) m
  guard $ length r42 > length r31

fix :: M.Map Int Condition -> M.Map Int Condition
fix = M.insert 8  (Or (If 42)
                      (And [If 42, If 8]))
    . M.insert 11 (Or (And [If 42, If 31])
                      (And [If 42, If 11, If 31]))
