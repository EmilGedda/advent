import Data.List
import Data.Char
import System.Environment
import System.Directory
import System.FilePath

-- advent-discover is used to create the yearly boilerplate files (e.g. Solutions.Y2020)

main :: IO ()
main = do
    src:_:dst:_ <- getArgs
    files <- getDirectoryContents $ dropExtension src

    let mod        = "Solutions." ++ takeBaseName src
        year       = map toLower $ takeBaseName src
        toImport m = "import " ++ mod ++ "." ++ m ++ " as S"
        modules    = map toImport  . filter solution $ map takeBaseName files

    writeFile dst $ unlines
                  [ "{-# LANGUAGE TemplateHaskell #-}"
                  , ""
                  , "module " ++ mod
                  , "    ( module S"
                  , "    , " ++ year
                  , "    ) where"
                  , ""
                  , "import Advent.Problem"
                  , "import Solutions.TH"
                  , ""
                  , unlines $ sort modules
                  , ""
                  , year ++ " :: Year"
                  , year ++ " = $(discoverDays)"
                  ]

solution :: FilePath -> Bool
solution ['D',a,b] = isDigit a && isDigit b && a < '3'
solution _ = False
