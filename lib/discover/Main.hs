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
        days s m = s ++ " " ++ mod ++ "." ++ m
        solutions  = filter solution $ map takeBaseName files
        imports    = unlines . sort $ map (days "import") solutions
        exports    = unlines . sort $ map ((++",") . days "module") solutions

    writeFile dst $ unlines
                  [ "{-# LANGUAGE TemplateHaskell #-}"
                  , ""
                  , "module " ++ mod
                  , "    ("
                  , exports
                  , year
                  , "    ) where"
                  , ""
                  , "import Advent.Problem"
                  , "import Solutions.TH"
                  , ""
                  , imports
                  , ""
                  , year ++ " :: Year"
                  , year ++ " = $(discoverDays)"
                  ]

solution :: FilePath -> Bool
solution ['D',a,b] = isDigit a && isDigit b && a < '3'
solution _ = False
