module Dtw
  (
  CostVal,
  dtwRecursiveSlow,
  dtwMemoising
  )
where

import Data.Maybe (Maybe)
import Data.Array (Array, array, (!), listArray)
import Data.Bifunctor (bimap)
import DtwMath (Unbounded, min3By)
import Data.Foldable (Foldable(fold))

class Ord x => CostVal x where
  zero :: x
  (.+.) :: x -> x -> x

instance CostVal Double where
  zero = 0.0
  (.+.) = (+)

instance CostVal Int where
  zero = 0
  (.+.) = (+)

-- |Given some bounds, find the possible states that could be used as previous states for the given state in the dynamic programming table
prevsFor :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
prevsFor (n, m) (0, 0) = []
prevsFor (n, m) (x, 0) = [(x-1, 0)]
prevsFor (n, m) (0, y) = [(0, y-1)]
prevsFor (n, m) (x, y)
  | 0 < x && x < n && 0 < y && y < m = [(x-1, y-1), (x-1, y), (x, y-1)]
  | otherwise = []

-- |Find the optimal previous state given a list of possible previous states
findMinPrev :: CostVal x => ((Int, Int) -> Maybe ([(Int, Int)], x)) -> [(Int, Int)] -> Maybe ([(Int, Int)], x)
findMinPrev recFunc = foldl foldFunc Nothing where
  foldFunc prev new@(x, y) = case recFunc (x, y) of
    Nothing -> prev
    Just (newSeq, newCost) -> case prev of
      Nothing -> Just (newSeq, newCost)
      Just (_, pc) -> if newCost < pc then Just (newSeq, newCost) else prev

-- |Calculate a minimal sequence of steps to link two sequences according and the total cost of the linking.
-- |Returns a list of pairs of indexes in the input lists.
-- |This implementation is performed recursively and doesn't memoise its results and so is very inefficient
dtwRecursiveSlow :: CostVal x => (a -> a -> x) -> [a] -> [a] -> Maybe ([(Int, Int)], x)
dtwRecursiveSlow _ [] _ = Nothing  -- Can't perform on empty list
dtwRecursiveSlow _ _ [] = Nothing  -- Can't perform on empty list
dtwRecursiveSlow cost xs ys = finaliseOutput (optimumAt (n-1, m-1)) where
  n = length xs
  m = length ys

  xsArr = array (0, n) (zip [0..] xs)
  ysArr = array (0, m) (zip [0..] ys)

  -- |Get the cost using indexes into the input lists as parameters
  costIdxs x y = cost (xsArr!x) (ysArr!y)

  -- |Perform final operations on the output to make it ready for returning
  finaliseOutput Nothing = Nothing
  finaliseOutput (Just (pairs, c)) = Just (
    reverse pairs,
    c
    )

  optimumAt (0, 0) = Just ([(0, 0)], zero)
  optimumAt (x, y) = case findMinPrev optimumAt (prevsFor (n, m) (x, y)) of
    Nothing -> Nothing
    Just (prevSeq, prevCost) -> Just ((x, y) : prevSeq, costIdxs x y .+. prevCost)

-- |Calculate a minimal sequence of steps to link two sequences according and the total cost of the linking.
-- |Returns a list of pairs of indexes in the input lists.
-- |This implementation uses memoisation to store intermediate results instead of having to recompute them
dtwMemoising :: CostVal x => (a -> a -> x) -> [a] -> [a] -> Maybe ([(Int, Int)], x)
dtwMemoising _ [] _ = Nothing  -- Can't perform on empty list
dtwMemoising _ _ [] = Nothing  -- Can't perform on empty list
dtwMemoising cost xs ys = finaliseOutput (dp!(n-1,m-1)) where
  n = length xs
  m = length ys

  xsArr = array (0, n) (zip [0..] xs)
  ysArr = array (0, m) (zip [0..] ys)

  finaliseOutput Nothing = Nothing
  finaliseOutput (Just (seq, cost)) = Just (reverse seq, cost)

  -- |Get the cost using indexes into the input lists as parameters
  costIdxs x y = cost (xsArr!x) (ysArr!y)

  dp = listArray ((0, 0), (n-1, m-1)) [optimumAt (i `div` m, i `mod` m) | i <- [0..(n*m)]]

  optimumAt (0, 0) = Just ([(0, 0)], zero)
  optimumAt (x, y) = case findMinPrev (dp!) (prevsFor (n, m) (x, y)) of
    Nothing -> Nothing
    Just (prevSeq, prevCost) -> Just ((x, y) : prevSeq, costIdxs x y .+. prevCost)

-- TODO - test if memoising implementation is actually being faster (otherwise was probably implemented wrong)
