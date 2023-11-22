module DtwTests where

import Test.HUnit
    ( assertEqual,
      runTestTT,
      Counts(failures),
      Test(TestList, TestCase, TestLabel), Assertion, AssertionPredicable (assertionPredicate), assertBool )
import System.Exit ( exitFailure, exitSuccess )
import GHC.Stack (HasCallStack)

import Dtw (dtwRecursiveSlow, CostVal)

type DtwFunc a x = (a -> a -> x) -> [a] -> [a] -> Maybe ([(Int, Int)], x)

-- Test case inputs

case1SeqA :: [Double]
case1SeqA = [1, 1, 1, 2, 3, 4]

case1SeqB :: [Double]
case1SeqB = [1, 2, 3, 4]

case1CostFunc :: Double -> Double -> Double
case1CostFunc x y = (x-y)^2

case2SeqA :: [Double]
case2SeqA = [1, 3, 5, 3, 3]

case2SeqB :: [Double]
case2SeqB = [5, 2, 2, 3, 5, 1]

case2CostFunc :: Double -> Double -> Double
case2CostFunc = max

-- Utility functions

assertEqualOneOf :: (HasCallStack, Eq a, Show a) => String -> [a] -> a -> Assertion
assertEqualOneOf msg exps out = assertBool msg (out `elem` exps)

getCasePart :: (Int, Int) -> ([a], [a]) -> ([a], [a])
getCasePart (x, y) (seqA, seqB) = (take (x+1) seqA, take (y+1) seqB)

genTest :: CostVal x => DtwFunc a x -> (a -> a -> x) -> [a] -> [a] -> (Int, Int) -> Maybe ([(Int, Int)], x)
genTest dtwFunc cost fullA fullB pos = let (seqA, seqB) = getCasePart pos (fullA, fullB) in dtwFunc cost seqA seqB

genRecSlowTest :: CostVal x => (a -> a -> x) -> [a] -> [a] -> (Int, Int) -> Maybe ([(Int, Int)], x)
genRecSlowTest = genTest dtwRecursiveSlow

genRecSlowCase1Test :: (Int, Int) -> Maybe ([(Int, Int)], Double)
genRecSlowCase1Test = genRecSlowTest case1CostFunc case1SeqA case1SeqB

genRecSlowCase2Test :: (Int, Int) -> Maybe ([(Int, Int)], Double)
genRecSlowCase2Test = genRecSlowTest case2CostFunc case2SeqA case2SeqB

-- Tests

-- Case 1 Tests

-- testRecSlowCase1? :: Test
-- testRecSlowCase1? = TestLabel "RecSlow-Case1-?" (TestCase
--     (
--         assertEqual ""
--         (Just ([  ], ?))
--         (genRecSlowCase1Test (?, ?))
--     ))

testRecSlowCase1Negative :: Test
testRecSlowCase1Negative = TestLabel "RecSlow-Case1-Negative" (TestCase
    (
        assertEqual ""
        Nothing
        (genRecSlowCase1Test (-1, -4))
    ))

testRecSlowCase1OutOfBounds :: Test
testRecSlowCase1OutOfBounds = TestLabel "RecSlow-Case1-OutOfBounds" (TestCase
    (
        assertEqual ""
        Nothing
        (genRecSlowCase1Test (4, 5))
    ))

testRecSlowCase1Full :: Test
testRecSlowCase1Full = TestLabel "RecSlow-Case1-Full" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (4,2), (5,3) ], 0))
        (genRecSlowCase1Test (5, 3))
    ))

testRecSlowCase1At01 :: Test
testRecSlowCase1At01 = TestLabel "RecSlow-Case1-0,1" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (0,1) ], 1))
        (genRecSlowCase1Test (0, 1))
    ))

testRecSlowCase1At50 :: Test
testRecSlowCase1At50 = TestLabel "RecSlow-Case1-5,0" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,0), (4,0), (5,0) ], 14))
        (genRecSlowCase1Test (5, 0))
    ))

testRecSlowCase1At33 :: Test
testRecSlowCase1At33 = TestLabel "RecSlow-Case1-3,3" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (3,2), (3,3) ], 5))
        (genRecSlowCase1Test (3, 3))
    ))

testRecSlowCase1At42 :: Test
testRecSlowCase1At42 = TestLabel "RecSlow-Case1-4,2" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (4,2) ], 0))
        (genRecSlowCase1Test (4, 2))
    ))

testRecSlowCase1At41 :: Test
testRecSlowCase1At41 = TestLabel "RecSlow-Case1-4,1" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (4,1) ], 1))
        (genRecSlowCase1Test (4, 1))
    ))

-- Case 2 Tests

-- testRecSlowCase2? :: Test
-- testRecSlowCase2? = TestLabel "RecSlow-Case2-?" (TestCase
--     (
--         assertEqual ""
--         (Just ([  ], ?))
--         (genRecSlowCase2Test (?, ?))
--     ))

testRecSlowCase2Negative :: Test
testRecSlowCase2Negative = TestLabel "RecSlow-Case2-Negative" (TestCase
    (
        assertEqual ""
        Nothing
        (genRecSlowCase2Test (-3, 1))
    ))

testRecSlowCase2OutOfBounds :: Test
testRecSlowCase2OutOfBounds = TestLabel "RecSlow-Case2-OutOfBounds" (TestCase
    (
        assertEqual ""
        Nothing
        (genRecSlowCase2Test (1, 8))
    ))

testRecSlowCase2Full :: Test
testRecSlowCase2Full = TestLabel "RecSlow-Case2-Full" (TestCase
    (
        assertEqual ""
        (Just 11)
        (fmap snd (genRecSlowCase2Test (4, 5)))
    ))

testRecSlowCase2At00 :: Test
testRecSlowCase2At00 = TestLabel "RecSlow-Case2-0,0" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0) ], 0))
        (genRecSlowCase2Test (0, 0))
    ))

testRecSlowCase2At14 :: Test
testRecSlowCase2At14 = TestLabel "RecSlow-Case2-1,4" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (0,1), (0,2), (0,3), (1,4) ], 5))
        (genRecSlowCase2Test (1, 4))
    ))

testRecSlowCase2At13 :: Test
testRecSlowCase2At13 = TestLabel "RecSlow-Case2-1,3" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (0,1), (0,2), (2,3) ], 4))
        (genRecSlowCase2Test (1, 3))
    ))

testRecSlowCase2At30 :: Test
testRecSlowCase2At30 = TestLabel "RecSlow-Case2-3,0" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,0) ], 3))
        (genRecSlowCase2Test (3, 0))
    ))

testRecSlowCase2At42 :: Test
testRecSlowCase2At42 = TestLabel "RecSlow-Case2-4,2" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (3,2) ], 8))
        (genRecSlowCase2Test (4, 2))
    ))

tests :: Test
tests = TestList
    [
        testRecSlowCase1Full,
        testRecSlowCase1At01,
        testRecSlowCase1At50,
        testRecSlowCase1At33,
        testRecSlowCase1At42,
        testRecSlowCase1At41
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then exitFailure else exitSuccess
