module Solutions.Y2020.D04 (day04) where

import Advent.Problem
import Control.Arrow            (second)
import Data.List                ((\\))
import Data.List.Split          (splitOn)
import qualified Data.Map as M

type Passport = [(String, String)]
newtype Passports = Passports { fromPassports :: [Passport] } deriving (Generic, NFData)

instance Parseable Passports where
    parseString = Passports . map (map (second (drop 1) . break (==':')) . words) . splitOn "\n\n"

day04 :: Day 4
day04 = day (length . passports) $ count (all validate) . passports

passports :: Passports -> [Passport]
passports = filter (null . (M.keys validators \\) . map fst) . fromPassports

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
