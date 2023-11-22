module DtwRecursiveSlowTests where

import Test.HUnit
    ( assertEqual,
      runTestTT,
      Counts(failures),
      Test(TestList, TestCase) )
import System.Exit ( exitFailure, exitSuccess )

import Dtw (dtwRecursiveSlow, CostVal)

-- Test case inputs

case1SeqA :: [Double]
case1SeqA = [1, 1, 1, 2, 3, 4]

case1SeqB :: [Double]
case1SeqB = [1, 2, 3, 4]

case1costFunc :: Double -> Double -> Double
case1costFunc x y = (x-y)^2

-- Utility functions

getCasePart :: (Int, Int) -> ([a], [a]) -> ([a], [a])
getCasePart (x, y) (seqA, seqB) = (take (x+1) seqA, take (y+1) seqB)

genTestPart :: CostVal x => (a -> a -> x) -> [a] -> [a] -> (Int, Int) -> Maybe ([(Int, Int)], x)
genTestPart cost fullA fullB pos = let (seqA, seqB) = getCasePart pos (fullA, fullB) in dtwRecursiveSlow cost seqA seqB

genCase1TestPart :: (Int, Int) -> Maybe ([(Int, Int)], Double)
genCase1TestPart = genTestPart case1costFunc case1SeqA case1SeqB

-- Tests

testCase1Full :: Test
testCase1Full = TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (4,2), (5,3) ], 0))
        (genCase1TestPart (5, 3))
    )

testCase1At01 :: Test
testCase1At01 = TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (0,1) ], 1))
        (genCase1TestPart (0, 1))
    )

testCase1At50 :: Test
testCase1At50 = TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,0), (4,0), (5,0) ], 14))
        (genCase1TestPart (5, 0))
    )

testCase1At33 :: Test
testCase1At33 = TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (3,2), (3,3) ], 5))
        (genCase1TestPart (3, 3))
    )

testCase1At42 :: Test
testCase1At42 = TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (4,2) ], 0))
        (genCase1TestPart (4, 2))
    )

testCase1At41 :: Test
testCase1At41 = TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (4,1) ], 1))
        (genCase1TestPart (4, 1))
    )

tests :: Test
tests = TestList
    [
        testCase1Full,
        testCase1At01,
        testCase1At50,
        testCase1At33,
        testCase1At42,
        testCase1At41
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then exitFailure else exitSuccess
