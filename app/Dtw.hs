module Dtw
  (
  dtwRecursiveSlow
  )
where

import Data.Maybe (Maybe)
import Data.Array (Array, array, (!))
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

-- -- |The starting index for the bottom-up results table
-- buStart :: (Int, Int)
-- buStart = (0, 0)

-- -- |Get the next index in the bottom-up results table for the DTW computation
-- buNext :: (Int, Int) -> (Int, Int)
-- buNext (x, y)
--     | x == y = (0, y+1)
--     | y > x = (y, x)
--     | x > y = (y+1, x)

-- -- |Generate a list of the indexes to be used for traversing the bottom-up results table given bounds for each axis
-- buIndexesList :: (Int, Int) -> [(Int, Int)]
-- buIndexesList (n, m) = (0, 0) : tailList (0, 0) where
--   tailList (x, y)
--     | (x == n) && (y == m) = []
--     | otherwise =
--       let next = buNext (x, y) in
--       next : tailList next

-- -- |Find the optimal sequence of pairs for the DTW calculation given a table of optimal value-predecessor pairs
-- backtrackFindOpt :: Ord x => Array (x, (Int, Int)) -> [(Int, Int)]
-- -- TODO

-- -- |Generate a table of optimal value-predecessor pairs for the DTW calculations
-- generateTable :: Ord x => (a -> a -> x) -> Array a -> Array b -> Array (x, (Int, Int))
-- generateTable cost xsArr ysArr = array (n, m) [findTableValue (ix, iy) | (ix, iy) <- buIndexesList (n, m)] where
--   -- TODO

-- -- |Calculate a minimal sequence of steps to link two sequences according to the provided cost function
-- dtw :: Ord x => (a -> a -> x) -> [a] -> [a] -> Maybe [(a, a)]
-- dtw _ [] _ = Nothing  -- Can't perform the operation on empty lists
-- dtw _ _ [] = Nothing  -- Can't perform the operation on empty lists
-- dtw cost xs ys = backtrackFindOpt (generateTable cost xsArr ysArr) where
--   n = length xs
--   m = length ys
--   tableSize = n * m
--   xsArr = array (0, n) (zip [0..] xs)
--   ysArr = array (0, m) (zip [0..] ys)

-- |Calculate a minimal sequence of steps to link two sequences according and the total cost of the linking.
-- |This implementation is performed recursively and doesn't memoise its results and so is very inefficient
dtwRecursiveSlow :: CostVal x => (a -> a -> x) -> [a] -> [a] -> Maybe ([(a, a)], x)
dtwRecursiveSlow _ [] _ = Nothing  -- Can't perform on empty list
dtwRecursiveSlow _ _ [] = Nothing  -- Can't perform on empty list
dtwRecursiveSlow cost xs ys = finaliseOutput (optimumAt (n, m)) where
  n = length xs
  m = length ys

  xsArr = array (0, n) (zip [0..] xs)
  ysArr = array (0, m) (zip [0..] ys)

  -- |Get the cost using indexes into the input lists as parameters
  costIdxs x y = cost (xsArr!x) (ysArr!y)

  -- |Perform final operations on the output to make it ready for returning
  finaliseOutput Nothing = Nothing
  finaliseOutput (Just (pairs, c)) = Just (
    reverse (map (bimap (xsArr!) (ysArr!)) pairs),
    c
    )

  optimumAt (0, 0) = Just ([(0, 0)], zero)
  optimumAt (x, y) = case findMinPrev (prevsFor (x, y)) of
    Nothing -> Nothing
    Just (prevSeq, prevCost) -> Just ((x, y) : prevSeq, costIdxs x y .+. prevCost)
    where

    prevsFor :: (Int, Int) -> [(Int, Int)]
    prevsFor (0, 0) = []
    prevsFor (x, 0) = [(x-1, 0)]
    prevsFor (0, y) = [(0, y-1)]
    prevsFor (x, y)
      | 0 <= x && x < n && 0 <= y && y < m = [(x-1, y-1), (x-1, y), (x, y-1)]
      | otherwise = []

    findMinPrev :: [(Int, Int)] -> Maybe ([(a, a)], x)
    findMinPrev [] = Nothing
