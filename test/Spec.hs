import Types
import Tests

import Prelude hiding       (null)
import Advent.Problem       (Day(..), Input, fromInput, fetchInput)
import Advent.Solution      (days)
import Data.ByteString      (null)
import Data.Either          (isRight)
import Control.Monad.Except (runExceptT)
import Data.Maybe           (catMaybes)
import Data.List            ((\\), find, intersect)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
    let solutions = map number days `intersect` map num answers
    tests <- map (apply testDay) <$> mapM findTest solutions
    defaultMain $ testGroup "Tests" [testConsistency, testGroup "Stars" tests]

testConsistency :: TestTree
testConsistency = testGroup "Test consistency" [
        testCase "No days lacking tests" $ map number days \\ map num answers @?= [],
        testCase "No tests lacking days" $ map num answers \\ map number days @?= []
    ]

findTest :: Integer -> IO (Answer, Day, Either String Input)
findTest n = do
    let Just answer = find ((==) n . num) answers
    let Just day = find ((==) n . number) days
    input <- runExceptT $ fetchInput 2020 n
    return (answer, day, input)

apply :: (a -> b -> c -> d) -> (a, b, c) -> d
apply f (a,b,c) = f a b c

testDay :: Answer -> Day -> Either String Input -> TestTree
testDay (Answer _ first second) (Day n partOne partTwo) input =
    let parts = zip3 "12" [partOne, partTwo] $ catMaybes [Just first, second]
        fromRight ~(Right r) = r
        fromLeft ~(Left a) = a
        day = "Day " ++ show n
        test (part, solution, answer) = after AllSucceed (day ++ ".Input") .
            testCase ("Part " ++ [part]) $ solution (fromRight input) @?= answer
    in testGroup day $
            testCase "Input" (do
                isRight input @? "Input is invalid: " ++ fromLeft input
                (not . null . fromInput $ fromRight input) @? "Input is empty"
        ):map test parts

