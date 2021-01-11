{-# LANGUAGE OverloadedStrings #-}
module Solutions.Y2020.D08 (day08) where

import           Advent.Problem                           (Day, Parseable, parseInput
                                                          , day, fromRight)
import           Control.Applicative                      ((<|>))
import           Data.Attoparsec.ByteString.Char8 hiding  (count, take)
import           Data.Functor                             (($>))
import qualified Data.Vector                      as V
import qualified Data.Set                         as S

data OpCode = Nop | Jmp | Acc deriving (Show, Eq, Enum)
data Instr = Instr OpCode Int deriving Show

data Computer = Computer {
                    accumulator :: Int,
                    index :: Int,
                    code :: V.Vector Instr,
                    len :: Int,
                    visited :: S.Set Int
                } deriving Show

opcode :: Parser OpCode
opcode = "nop" $> Nop <|> "jmp" $> Jmp <|> "acc" $> Acc

instruction :: Parser Instr
instruction = Instr <$> opcode <*> (" " *> signed decimal)

instance Parseable Instr where
    parseInput = fromRight . parseOnly instruction

day08 :: Day
day08 = day 8 (accumulator . run . computer) (accumulator . partTwo)

partTwo :: V.Vector Instr -> Computer
partTwo bootcode = V.head . V.filter ((==) <$> index <*> len)
                 . V.map (run . computer . fix bootcode)
                 . replaceable $ bootcode

computer :: V.Vector Instr -> Computer
computer instrs = Computer 0 0 instrs (length instrs) S.empty

fix :: V.Vector Instr -> Int -> V.Vector Instr
fix xs n = xs V.// [(n, replace $ xs V.! n)]

replaceable :: V.Vector Instr -> V.Vector Int
replaceable = V.findIndices (\(Instr op _) -> op `elem` [Jmp, Nop])

run :: Computer -> Computer
run c@(Computer _ idx code n visited)
  | S.member idx visited || idx >= n = c
  | otherwise = run $ exec (code V.! idx) c

exec :: Instr -> Computer -> Computer
exec (Instr Acc v) c@(Computer acc _ _ _ _)
  = step 1 $ c{ accumulator = acc + v }
exec (Instr Jmp n) c = step n c
exec (Instr Nop _) c = step 1 c

step :: Int -> Computer -> Computer
step n c@(Computer _ idx _ _ v)
  = c{ index = idx + n, visited = S.insert idx v }

replace :: Instr -> Instr
replace (Instr Nop v) = Instr Jmp v
replace (Instr Jmp v) = Instr Nop v
