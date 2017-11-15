module Homework where
--
import Data.List
import Data.Char
import Data.Function ( fix )
--

-- Task 01

-- non accumulator style
factorial :: (Num a, Eq a) => a -> a
factorial = fix (\f x -> if x == 0 then 1 else x * f (x - 1))

-- non accumulator style
sum' :: Num a => [a] -> a
sum' = fix (\f xs -> case xs of
                          x:xs -> x + f xs
                          []   -> 0)

-- accumulator style
factorial' :: (Num a, Eq a) => a -> a
factorial' x = fix (\f x acc -> if x == 0 then acc else f (x - 1) (acc * x)) x 1

-- accumulator style
sum'' :: Num a => [a] -> a
sum'' xs = fix (\f xs acc -> case xs of
                            x:xs -> f xs (acc + x)
                            []   -> acc) xs 0

nats :: [Integer]
nats = fix (\f x -> x:f (x + 1)) 1

map' :: (a -> b) -> [a] -> [b]
map' f = fix (\rec xs -> case xs of
                              x:xs -> f x: rec xs
                              []   -> [])

zip' :: [a] -> [b] -> [(a, b)]
zip' = fix (\f xs ys -> case xs of
                                x:xs -> case ys of
                                          y:ys -> (x, y):f xs ys
                                          []   -> []
                                []   -> [])

-- Task 02
subsets :: Eq a => Int -> [a] -> [[a]]
subsets k xs
  | k > length xs = [[]]
  | otherwise     = subsetsRecursive k (nub xs) [[]]
  where
    combine ss xs             = [x:ss | x <- xs, x `notElem` ss]
    subsetsRecursive 0 _ acc  = acc
    subsetsRecursive k xs acc =
      subsetsRecursive (k - 1) xs (nubBy equalSubsets subsets)
                         where subsets = foldr (\ss res -> res ++ combine ss xs) [] acc

equalSubsets :: Eq a => [a] -> [a] -> Bool
equalSubsets ss1 = all (`elem` ss1)

partitions :: Eq a => [a] -> [[[a]]]
partitions [] = error "Empty set has no partititons"
partitions xs = partitionsRecursive $ nub xs
  where
    partitionsRecursive [] = [[]]
    partitionsRecursive (x:xs) = map ([x]:) tailPartitions ++
      [(x:ys):yss | (ys:yss) <- tailPartitions]
        where tailPartitions = partitionsRecursive xs

-- Task 03
permutations' :: Eq a => [a] -> [[a]]
permutations'  [] = [[]]
permutations' xs = concat [[x:perm | perm <- permutations' $ filter (/= x) xs] | x <- nub xs]

