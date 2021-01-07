import Types
import Tests

import Prelude hiding       (null)
import Advent.Problem       (Day(..), Year(..), Input, fromInput, fetchInput, solution, parseInput, fromInput)
import Solutions            (years)
import Data.ByteString      (null)
import Data.Either          (isRight)
import Control.Monad.Except (runExceptT, forM)
import Data.Maybe           (catMaybes)
import Data.List            ((\\), find, intersect)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
    defaultMain
        . testGroup "Tests"
        =<< forM years
            (\(Year y d) ->
                let Just ans = solutions <$> find ((==y) . when) answers
                in testGroup (show y)
                   . (testConsistency d ans:)
                   . return
                   . testGroup "Stars"
                   . map (apply testDay)
                   <$> mapM (findTest d ans)
                   (map number d `intersect` map num ans))


testConsistency :: [Day] -> [Answer] -> TestTree
testConsistency d ans = testGroup "Test consistency" [
        testCase "No days lacking tests" $ map number d \\ map num ans @?= [],
        testCase "No tests lacking days" $ map num ans \\ map number d @?= []
    ]

findTest :: [Day] -> [Answer] -> Integer -> IO (Answer, Day, Either String Input)
findTest days answers n = do
    let Just ans = find ((==) n . num) answers
    let Just d = find ((==) n . number) days
    input <- runExceptT $ fetchInput 2020 n
    return (ans, d, input)

apply :: (a -> b -> c -> d) -> (a, b, c) -> d
apply f (a,b,c) = f a b c

testDay :: Answer -> Day -> Either String Input -> TestTree
testDay (Answer _ first second) (Day n partOne partTwo) input =
    let wrap f = solution . f . parseInput . fromInput
        parts = zip3 "12" [wrap partOne, wrap partTwo] $ catMaybes [Just first, second]
        fromRight ~(Right r) = r
        fromLeft ~(Left a) = a
        day = "Day " ++ show n
        test (part, output, answer) = after AllSucceed (day ++ ".Input") .
            testCase ("Part " ++ [part]) $ output (fromRight input) @?= solution answer
    in testGroup day $
            testCase "Input" (do
                isRight input @? "Input is invalid: " ++ fromLeft input
                (not . null . fromInput $ fromRight input) @? "Input is empty"
        ):map test parts

