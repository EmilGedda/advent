module Advent.Solution.DayFour (day4) where

import Advent.Problem           (Day, day, Parseable(..), count, between)
import Control.Arrow            (second)
import Data.List                ((\\))
import Data.List.Split          (splitOn)
import qualified Data.Map as M

newtype Passports = Passports [Passport]
newtype Passport = Passport {
                        fromPassport :: [(String, String)]
                    }

instance Parseable Passport where
    parseString = Passport . map (second (drop 1) . break (==':')) . words

instance Parseable Passports where
    parseString = Passports . map parseString . splitOn "\n\n"

day4 :: Day
day4 = day 4 (length . passports) $ count (all validate . fromPassport) . passports

passports :: Passports -> [Passport]
passports (Passports p) = filter (null . (M.keys validators \\) . map fst . fromPassport) p

validate :: (String, String) -> Bool
validate (k, v) = maybe True ($ v) $ validators M.!? k

validators :: M.Map String (String -> Bool)
validators =
        let
            year lower upper y  = length y == 4 && between lower upper y
            height (a:b:c:"cm") = between "150" "193" [a,b,c]
            height (x:y:"in")   = between "59" "76" [x,y]
            height _            = False
            hair ('#':color) = length color == 6 && all (`elem` "0123456789abcdef") color
            hair _           = False
            eye   = flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
            pid n = length n == 9 && all (`elem` "0123456789") n

        in M.fromList [
            ("byr", year "1920" "2002"),
            ("iyr", year "2010" "2020"),
            ("eyr", year "2020" "2030"),
            ("hgt", height),
            ("hcl", hair),
            ("ecl", eye),
            ("pid", pid)
        ]
