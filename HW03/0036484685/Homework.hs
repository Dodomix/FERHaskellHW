module Homework where
--
import Data.List
import Data.Char
import Data.Bits ( xor )
--

-- Task 01
localMaxima :: [Int] -> [Int]
localMaxima (x1:xs@(x2:x3:_))
  | x2 > x1 && x2 > x3 = (x2:rest)
  | otherwise          = rest
  where rest           = localMaxima xs
localMaxima _          = []

-- Task 02
transform :: [(Int, String)] -> [(Char, Int)]
transform scores = transformRecursive scores []
  where
    transformRecursive (score:scores) acc = transformRecursive scores $ acc ++ transformScore score
    transformRecursive _ acc              = acc
    transformScore (points, letters)      = [(toLower letter, points) | letter <- letters]

-- Task 03
rule90 :: [Bool] -> [[Bool]]
rule90 xs = xs : rule90 newXs
  where newXs = rule90Step xs

rule90Step :: [Bool] -> [Bool]
rule90Step xs = rule90StepRecursive False xs
  where
    rule90StepRecursive predecessor (x:succ:xs) = predecessor `xor` succ : rule90StepRecursive x (succ:xs)
    rule90StepRecursive predecessor [x] = [predecessor `xor` False]
    rule90StepRecursive _ _ = []

pretty :: [[Bool]] -> String
pretty (x:xs) = prettyStep x ++ "\n" ++ pretty xs
  where prettyStep xs = [prettyChar x | x <- xs]
        prettyChar x
          | x = '#'
          | otherwise = ' '
pretty [] = ""

-- Task 04
f :: [String]
f = "1" : next f
  where
    next (x:xs)         = lookAndSay x : next xs
    lookAndSay xs@(x:_) = (show $ length $ takeWhile (== x) xs) ++
      [x] ++
      (lookAndSay $ dropWhile (== x) xs)
    lookAndSay ""       = ""
