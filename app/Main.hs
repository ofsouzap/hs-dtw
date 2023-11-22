module Main where

import Text.Show ()
import Dtw (dtwRecursiveSlow, dtwMemoising)
import GHC.Arr ((!), listArray)

seqA :: [Int]
seqA = [1, 1, 1, 2, 3, 4]

seqB :: [Int]
seqB = [1, 2, 3, 4]

costFunc :: Int -> Int -> Int
costFunc x y = (x-y)^2

listToString :: Show a => [a] -> String
listToString [] = ""
listToString (h:ts) = show h ++ "," ++ listToString ts

runFor :: (Int, Int) -> Maybe ([(Int, Int)], Int)
runFor (x, y) = dtwMemoising costFunc (take (x+1) seqA) (take (y+1) seqB)

outputFor :: Maybe ([(Int, Int)], Int) -> String
outputFor x = case x of
    Nothing -> "Nothing found"
    Just (seq, cost) -> listToString seq ++ " [cost=" ++ show cost ++ "]"

doOutput :: (Int, Int) -> IO ()
doOutput (x, y) = do
    putStr ("(" ++ show x ++ ", " ++ show y ++ ") => ")
    (putStrLn . outputFor . runFor) (x, y)

main :: IO ()
main = do
    let (n,m) = (3,2) in
        print ((listArray ((0,0),(n-1,m-1)) [(i `div` m, i `mod` m) | i <- [0..(n*m)]])!(2,1))
    -- doOutput (0, 0)
    -- doOutput (1, 2)
    -- doOutput (3, 1)
    -- doOutput (5, 3)
