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
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-04.lhs
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-05.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 04-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-04.lhs

-- EXERCISE 01 =======================================================================

-- Define 'headHunter xss' that takes the head of the first list element. If
-- the first element has no head, it takes the head of the second element.
-- If the second element has no head, it takes the head of the third element.
-- If none of this works, the function returns an error.
ex411 :: [[a]] -> a
ex411 = headHunter
headHunter :: [[a]] -> a
headHunter ((x:_):_)       = x
headHunter ([]:(x:_):_)    = x
headHunter ([]:[]:(x:_):_) = x
headHunter _               = error "The first three elements are already decapitated"

-- Define 'firstColumn m' that returns the first column of a matrix.
-- firstColumn [[1,2],[3,4]] => [1,3]
-- Check what happens if the input is not a valid matrix.
ex412 :: [[a]] -> [a]
ex412 = firstColumn
firstColumn :: [[a]] -> [a]
firstColumn m@(firstRow:_) = [if length row == length firstRow then head row else error "Received an invalid matrix" | row <- m]
firstColumn _              = []

-- Define 'shoutOutLoud' that repeats three times the initial letter of each
-- word in a string.
-- shoutOutLoud :: String -> String
-- shoutOutLoud "Is anybody here?" => "IIIs aaanybody hhhere?"
ex413 :: String -> String
ex413 = shoutOutLoud
shoutOutLoud :: String -> String
shoutOutLoud s = unwords [[x] ++ [x] ++ [x] ++ rest | (x:rest) <- words s]

-- EXERCISE 02 =======================================================================

-- Define 'pad' that pads the shorter of two the strings with trailing spaces
-- and returns both strings capitalized.
-- pad :: String -> String -> (String, String)
-- pad "elephant" "cat" => ("Elephant", "Cat     ")
ex421 :: String -> String -> (String, String)
ex421 = pad
pad :: String -> String -> (String, String)
pad (h1:t1) (h2:t2)
  | length s1 > length s2 = (s1, s2 `padTo` length s1)
  | otherwise             = (s1 `padTo` length s2, s2)
  where
    padTo s len        = s ++ getBlankString len
    getBlankString len = concat $ take len $ repeat " "
    s1                 = toUpper h1:t1
    s2                 = toUpper h2:t2
pad _ _                = error "One or both strings are empty"

-- Define 'quartiles xs' that returns the quartiles (q1,q2,q3) of a given list.
-- The quartiles are elements at the first, second, and third quarter of a list
-- sorted in ascending order. (You can use the built-int 'splitAt' function and
-- the previously defined 'median' function.)
-- quartiles :: [Int] -> (Double,Double,Double)
-- quartiles [3,1,2,4,5,6,8,0,7] => (1.5, 4.0, 6.5)

median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs
  | odd l     = realToFrac $ ys !! h
  | otherwise = realToFrac (ys !! h + ys !! (h - 1)) / 2
  where l  = length xs
        h  = l `div` 2
        ys = sort xs

ex422 :: [Int] -> (Double, Double, Double)
ex422 = quartiles
quartiles :: [Int] -> (Double, Double, Double)
quartiles [] = error "quartiles: Empty list"
quartiles qs = (median $ fst split, median qs, median $ snd split)
  where sorted     = sort qs
        l          = length sorted
        halfLength = l `div` 2
        split      = if odd l then
                       (fst $ splitAt (halfLength) sorted, -- before the median
                        snd $ splitAt (halfLength + 1) sorted) -- after the median
                     else splitAt halfLength sorted

-- EXERCISE 03 =======================================================================

-- Redo Exercise 2 using 'let' instead of 'where'.
ex431 :: String -> String -> (String, String)
ex431 = pad'
pad' :: String -> String -> (String, String)
pad' (h1:t1) (h2:t2) =
  let
    getBlankString len = concat $ take len $ repeat " "
    padTo s len        = s ++ getBlankString len
    s1                 = toUpper h1:t1
    s2                 = toUpper h2:t2
  in
    if length s1 > length s2 then (s1, s2 `padTo` length s1)
    else (s1 `padTo` length s2, s2)
pad' _ _             = error "One or both strings are empty"

ex432 :: [Int] -> (Double, Double, Double)
ex432 = quartiles'
quartiles' :: [Int] -> (Double, Double, Double)
quartiles' [] = error "quartiles: Empty list"
quartiles' qs =
  let
    sorted     = sort qs
    l          = length sorted
    halfLength = l `div` 2
    split      = if odd l then
                   (fst $ splitAt (halfLength) sorted, -- before the median
                    snd $ splitAt (halfLength + 1) sorted) -- after the median
                 else splitAt halfLength sorted
  in (median $ fst split, median qs, median $ snd split)

-- EXERCISE 04 =======================================================================

-- Write a function that takes in a pair (a,b) and a list [c] and returns the
-- following string:
-- "The pair [contains two ones|contains one one|does not contain a single one]
-- and the second element of the list is <x>"

ex441 :: (Show a) => (Int, Int) -> [a] -> String
ex441 pair (_:z:_) =
  "The pair " ++
    (case pair of
      (1, 1) -> "contains two ones"
      (_, 1) -> "contains one one"
      (1, _) -> "contains one one"
      (_, _) -> "does not contain a single one") ++
        " and the second element of the list is " ++ show z

{-LECTURE 05-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-05.lhs

-- EXERCISE 01 =======================================================================

-- Define a recursive function to compute the product of a list of elements.
-- product' :: Num a => [a] -> a
ex511 :: Num a => [a] -> a
ex511 = product'
product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

-- Define a recursive function 'headsOf' that takes a list of lists and
-- returns a list of their heads.
-- headsOf :: [[a]] -> [a]
-- headsOf [[1,2,3],[4,5],[6]] => [1,4,6]
ex512 :: [[a]] -> [a]
ex512 = headsOf
headsOf :: [[a]] -> [a]
headsOf []          = []
headsOf ([]:xss)    = headsOf xss
headsOf ((x:_):xss) = [x] ++ headsOf xss

-- EXERCISE 02 =======================================================================

-- Define a recursive function 'modMult n m xs' that multiplies each element of
-- a list 'xs' with 'n' modulo 'm'.

ex521 :: Integer -> Integer -> [Integer] -> [Integer]
ex521 = modMult
modMult :: Integer -> Integer -> [Integer] -> [Integer]
modMult _ _ []     = []
modMult n m (x:xs) = x * (n `mod` m) : modMult n m xs

-- Define a function 'addPredecessor' that adds to each element of a list the
-- value of the preceding element. The first element gets no value added.
-- addPredecessor :: Num a => [a] -> [a]
-- addPredecessor [3,2,1] => [3,5,3]
ex522 :: Num a => [a] -> [a]
ex522 = addPredecessor
addPredecessor :: Num a => [a] -> [a]
addPredecessor xs    = add 0 xs
  where add _ []     = []
        add n (x:xs) = x + n : add x xs

-- EXERCISE 03 =======================================================================

-- Define 'equalTriplets' that filters from a list of triplets (x,y,z) all
-- triplets for which x==y==z.
-- equalTriplets [(1,2,3),(2,2,2),(4,5,6)] => [(2,2,2)]
ex531 :: Eq a => [(a, a, a)] -> [(a, a, a)]
ex531 = equalTriplets
equalTriplets :: Eq a => [(a, a, a)] -> [(a, a, a)]
equalTriplets []                             = []
equalTriplets ((x, y, z):xs)
  | x == y && y == z = (x, y, z) : equalTriplets xs
  | otherwise     = equalTriplets xs

-- Define your own version of the replicate function:
-- replicate' :: Int -> a -> [a]
ex532 :: Int -> a -> [a]
ex532 = replicate'
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a
  | n > 0 = a : replicate' (n - 1) a
  | otherwise = []

-- EXERCISE 04 =======================================================================

-- Define your own recursive version of the drop function:
-- drop' :: Int -> [a] -> [a].
-- Define drop'' (a wrapper function) so that for n < 0 the function drops
-- the elements from the end of the list. You can use 'reverse'.

ex541 :: Int -> [a] -> [a]
ex541 = drop'
drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = [] -- we already dropped all elements
drop' n (x:xs) = drop' (n - 1) xs

ex541' :: Int -> [a] -> [a]
ex541' = drop''
drop'' :: Int -> [a] -> [a]
drop'' n xs
  | n < 0     = reverse $ drop' (-n) $ reverse xs
  | otherwise = drop' n xs

-- Define a recursive function 'takeFromTo n1 n2 xs'.
-- takeFromTo :: Int -> Int -> [a] -> [a]

ex542 :: Int -> Int -> [a] -> [a]
ex542 = takeFromTo
takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo 0 end xs         = take (end + 1) xs -- take +1 because it's inclusive
takeFromTo start end (x:xs) =
  takeFromTo (realStart - 1) (realEnd - 1) xs
    where realStart = min start end
          realEnd = max start end -- reverse if start is larger than end

-- EXERCISE 05 =======================================================================

-- Define a recursive function 'eachThird' that retains every third element
-- in a list.
-- eachThird :: [a] -> [a]
-- eachThird "zagreb" => "gb"

ex551 :: [a] -> [a]
ex551 = eachThird
eachThird :: [a] -> [a]
eachThird (_:_:x:xs) = x : eachThird xs
eachThird _          = []

-- Define a recursive function 'crossZip' that zips two lists in a "crossing"
-- manner:
-- crossZip [1,2,3,4,5] [4,5,6,7,8] => [(1,5),(2,4),(3,7),(4,6)]
ex552 :: [a] -> [b] -> [(a, b)]
ex552 = crossZip
crossZip :: [a] -> [b] -> [(a, b)]
crossZip (x1:x2:xs) (y1:y2:ys) = (x1, y2) : (x2, y1) : crossZip xs ys
crossZip _ _                   = []

-- EXERCISE 06 =======================================================================

-- Write an accumulator-style recursive definition of
-- length' :: [a] -> Int

ex561 = length'
length' = undefined

-- Write an accumulator-style recursive definition of
--     maxUnzip :: [(Int, Int)] -> (Int, Int)
-- that returns the maximum element at the first position and the maximum
-- element at the second position in a pair, i.e., it's equivalent to:
--     maxUnzip zs = (maximum xs, maximum ys)
--         where (xs,ys) = unzip zs
-- If the list is empty, return an "empty list" error.
-- Now write a standard recursive definition maxUnzip' (without an accumulator).
ex562 = maxUnzip
maxUnzip = undefined

ex562' = maxUnzip'
maxUnzip' = undefined
