{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--
import Data.List
import Data.Char
--

{-
    Here you should provide your solutions to in-class exercises.

    Make sure that ALL FUNCTIONS (including exXXX) have correct TYPE SIGNATURES.

    You should include solutions from following lectures :
    - http://www.fer.unizg.hr/_download/repository/puh-2017-lecture-06.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 06-} -- http://www.fer.unizg.hr/_download/repository/puh-2017-lecture-06.lhs

-- EXERCISE 01 =======================================================================

{-
  1.1.
  - Write an accumulator-style recursive definition of
    length' :: [a] -> Int
-}

ex611 :: [a] -> Int
ex611 = length'
length' :: [a] -> Int
length' xs = len xs 0
  where len [] n     = n
        len (_:xs) n = len xs (n+1)

{-
  1.2
  - Write an accumulator-style recursive definition of
      maxUnzip :: [(Int, Int)] -> (Int, Int)
    that returns the maximum element at the first position and the maximum
    element at the second position in a pair, i.e., it's equivalent to:
      maxUnzip zs = (maximum xs, maximum ys)
        where (xs,ys) = unzip zs
    If the list is empty, return an "empty list" error.

  - Now write a standard recursive definition (without an accumulator).
-}
ex612 :: [(Int, Int)] -> (Int, Int)
ex612 = maxUnzip
maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "empty list"
maxUnzip ((x, y):zs) = maxUnzip' zs x y
  where maxUnzip' [] n m = (n, m)
        maxUnzip' ((x, y):zs) n m = maxUnzip' zs (max x n) (max y m)

ex662' :: [(Int, Int)] -> (Int, Int)
ex662' = maxUnzip'
maxUnzip' :: [(Int, Int)] -> (Int, Int)
maxUnzip' [] = error "empty list"
maxUnzip' [(x, y)] = (x, y)
maxUnzip' ((x, y):zs) = (max x $ fst maxRest, max y $ snd maxRest)
  where maxRest = maxUnzip' zs
