module Main where

import Test.HUnit
    ( assertEqual,
      runTestTT,
      Counts(failures),
      Test(TestList, TestCase) )
import System.Exit ( exitFailure, exitSuccess )
import DtwTests

sanityTest :: Test
sanityTest = TestCase (assertEqual "should be 3" 3 (1 + 2))

allTests :: Test
allTests = TestList
    ([
        sanityTest,
        DtwTests.tests
    ])

main :: IO ()
main = do
    result <- runTestTT allTests
    if failures result > 0 then exitFailure else exitSuccess
