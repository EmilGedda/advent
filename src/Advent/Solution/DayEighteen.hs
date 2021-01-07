{-# LANGUAGE OverloadedStrings #-}
module Advent.Solution.DayEighteen where

import Advent.Problem (day, Day, Parseable(..), notSolved, fromRight, debug)
import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8
import Debug.Trace

newtype Equation = Equation {fromEquation :: BS.ByteString}

instance Parseable Equation where
    parseInput = Equation . BS.map switch . BS.reverse

data Expr = Add Expr Expr
          | Mul Expr Expr
          | Const Int
          deriving Show

term p = "(" *> p <* ")" <|> Const <$> decimal

grammar1 = binop Add " + " <|> binop Mul " * " <|> term grammar1
    where binop e token = e <$> term grammar1 <*> (token *> grammar1)

grammar2 = Mul <$> add <*> (" * " *> grammar2) <|> add
    where add = Add <$> term grammar2 <*> (" + " *> add) <|> term grammar2

switch ')' = '('
switch '(' = ')'
switch x = x

runExpr :: Expr -> Int
runExpr (Add a b) = runExpr a + runExpr b
runExpr (Mul a b) = runExpr a * runExpr b
runExpr (Const x) = x

day18 :: Day
day18 = day 18 (eval grammar1) (eval grammar2)

eval :: Parser Expr -> [Equation] -> Int
eval parser = sum . map (runExpr . fromRight . parseOnly parser . fromEquation)

