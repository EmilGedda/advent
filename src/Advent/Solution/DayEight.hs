{-# LANGUAGE OverloadedStrings #-}
module Advent.Solution.DayEight where

import           Advent.Problem                           (Day, Parseable, parseInput, day, fromRight)
import           Control.Applicative                      ((<|>))
import           Data.Attoparsec.ByteString.Char8 hiding  (count, take)
import           Data.Functor                             (($>))
import           Data.Maybe                               (mapMaybe)
import qualified Data.Set                         as S

data OpCode = Nop | Jmp | Acc deriving (Show, Eq)
data Instr = Instr OpCode Int deriving Show

data Computer = Computer {
                    accumulator :: Int,
                    index :: Int,
                    code :: [Instr],
                    len :: Int,
                    visited :: S.Set Int
                } deriving Show

opcode :: Parser OpCode
opcode = "nop" $> Nop
     <|> "jmp" $> Jmp
     <|> "acc" $> Acc

instruction = Instr <$> opcode <*> (" " *> signed decimal)

instance Parseable Instr where
    parseInput = fromRight . parseOnly instruction

day8 :: Day
day8 = day 8 partOne partTwo

partTwo :: [Instr] -> Int
partTwo bootcode = head . mapMaybe (verify . run . construct . fix bootcode)
                 . replaceable $ zip [0..] bootcode
    where construct instrs = Computer 0 0 instrs (length bootcode) S.empty
          verify (Computer acc idx _ n _)
            | idx == n  = Just acc
            | otherwise = Nothing

partOne :: [Instr] -> Int
partOne bootcode = accumulator $ run (Computer 0 0 bootcode (length bootcode) S.empty)


fix :: [Instr] -> Int -> [Instr]
fix xs n = take n xs ++ replace (xs !! n):drop (n + 1) xs

replaceable :: [(a, Instr)] -> [a]
replaceable = map fst . filter (swapped . snd)
        where swapped (Instr op _) = op == Nop || op == Jmp

run c@(Computer _ idx code n visited)
  | S.member idx visited || idx >= n = c
  | otherwise = run $ exec (code !! idx) c

exec :: Instr -> Computer -> Computer
exec (Instr Acc v) c@(Computer acc _ _ _ _) = (step 1 c){ accumulator = acc + v }
exec (Instr Jmp n) c = step n c
exec (Instr Nop _) c = step 1 c

visit :: Computer -> Computer
visit c@(Computer _ idx _ _ v) = c{ visited = S.insert idx v }

step :: Int -> Computer -> Computer
step n c@(Computer _ idx _ _ _) = (visit c){ index = idx + n }

replace (Instr Nop v) = Instr Jmp v
replace (Instr Jmp v) = Instr Nop v
replace x = x
