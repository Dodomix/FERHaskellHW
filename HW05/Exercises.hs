{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--

{-
    Here you should provide your solutions to in-class exercises.

    Make sure that ALL FUNCTIONS (including exXXX) have correct TYPE SIGNATURES.

    You should include solutions from following lectures :
    http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-08.lhs
    http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-09.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 08-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-08.lhs

import Data.List
import Data.Char
import Data.Ord (comparing)

-- EXERCISE 01 =======================================================================
{-
  Define the following functions using composition and pointfree style (you may
  of course use local definitions):
-}

{-
  1.1.
  - Define 'sumEven' that adds up elements occurring at even (incl. zero)
    positions in a list.
    sumEven :: [Integer] -> Integer
    sumEven [1..10] => 25
-}

sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . zip [0..]

{-
  1.2.
  - Define 'filterWords ws s' that removes from string 's' all words contained
    in the list 'ws'.
    filterWords :: [String] -> String -> String
-}

filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (`notElem` ws) . words

{-
  1.3.
  - Define 'initials3 d p s' that takes a string 's' and turns it into a string
    of initials. The function is similar to 'initials2' but additionally delimits
    the initials with string 'd' and discards the initials of words that don't
    satisfy the predicate 'p'.
    initials3 :: String -> (String -> Bool) -> String -> String
    initials3 "." (/="that") "a company that makes everything" => "A.C.M.E."
  - Use this function to define the 'initials' function.
-}

initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p = concat . map ((:d) . toUpper . head) .  filter p . words

-- EXERCISE 02 =======================================================================
{-
  Just a reminder that EVERY function in this file needs to have a type signature ;)
-}

{-
  2.1.
  - Define 'maxDiff xs' that returns the maximum difference between consecutive
    elements in the list 'xs'.
    maxDiff :: [Int] -> Int
    maxDiff [1,2,3,5,1] => 4
  - Define 'maxMinDiff' that returns the pair (min_difference, max_difference).
-}

maxDiff :: [Int] -> Int
maxDiff xs = maximum . map (abs . uncurry (-)) . zip xs $ tail xs

maxMinDiff :: [Int] -> (Int, Int)
maxMinDiff xs = (minDiff xs, maxDiff xs)
  where minDiff xs = minimum . map (abs . uncurry (-)) . zip xs $ tail xs

{-
  2.2.
  - Define 'studentsPassed' that takes as input a list [(NameSurname,Score)] and
    returns the names of all students who scored at least 50% of the maximum
    score.
-}

studentsPassed :: (Fractional a, Ord a) => [(String, a)] -> [String]
studentsPassed xs = (map fst . filter ((halfMaxScore <=) . snd)) xs
  where halfMaxScore = 0.5 * ((maximum . map snd) xs)

-- EXERCISE 03 =======================================================================
{-
  3.1.
  - Define 'isTitleCased' that checks whether every word in a string is
    capitalized.
    isTitleCased :: String -> Bool
    isTitleCased "University Of Zagreb" => True
-}

isTitleCased :: String -> Bool
isTitleCased = all (isUpper . head) . words

{-
  3.2.
  - Define 'sortPairs' that sorts the list of pairs in ascending order with
    respect to the second element of a pair.
-}

sortPairs :: Ord b => [(a, b)] -> [(a, b)]
sortPairs = sortBy (comparing snd)

{-
  3.3.
  - Define 'filename' that extracts the the name of the file from a file path.
    filename :: String -> String
    filename "/etc/init/cron.conf" => "cron.conf"
-}

filename :: String -> String
filename f = tail . (drop $ last $ findIndices (== '/') f) $ f

{-
  3.4.
  - Define 'maxElemIndices' that returns the indices of the maximum element in a
    list. Return "empty list" error if the list is empty.
    maxElemIndices :: Ord a => [a] -> [Int]
    maxElemIndices [1,3,4,1,3,4] => [2,5]
-}

maxElemIndices :: Ord a => [a] -> [Int]
maxElemIndices [] = error "empty list"
maxElemIndices xs = findIndices (== maxX) xs
  where maxX = maximum xs

-- EXERCISE 04 =======================================================================

{-
  4.1.
  - Define 'elem'' using 'foldr'.
-}

elem :: Eq a => a -> [a] -> Bool
elem x = foldr (\y acc -> acc || x == y) False

{-
  4.2.
  - Define 'reverse' using 'foldr'.
-}

reverse :: [a] -> [a]
reverse = foldr (\x acc -> acc ++ [x]) []

{-
  4.3.
  - Using 'foldr' define 'nubRuns' that removes consecutively repeated elements
    from a list.
    nubRuns :: Eq a => [a] -> [a]
    nubRuns "Mississippi" => "Misisipi"
-}

nubRuns :: Eq a => [a] -> [a]
nubRuns = foldr appendIfNotHead []
  where appendIfNotHead x []        = [x]
        appendIfNotHead x acc@(y:_) = if x == y then acc else x:acc

-- EXERCISE 05 =======================================================================

{-
  5.1.
  - Write 'reverse' using 'foldl'.
    reverse' :: [a] -> [a]
-}

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

{-
  5.2.
  - Using 'foldl' define function 'sumEven' from problem 1.1.
-}

sumEven' :: [Integer] -> Integer
sumEven' = foldl (\acc (i, a) -> if even i then acc + a else acc) 0 . zip [0..]

{-
  5.3.
  - Using 'foldl' define maxUnzip :: [(Int,Int)] -> (Int,Int)
    that returns the maximum element at first position in a pair and maximum
    element at second position in the pair. In other words, the function should
    be equivalent to:
      maxUnzip zs = (maximum xs, maximum ys)
        where (xs,ys) = unzip zs
    Return "empty list" error if the list is empty.
-}

maxUnzip :: [(Int,Int)] -> (Int,Int)
maxUnzip [] = error "empty list"
maxUnzip xs = foldl1 (\(acc1, acc2) (x1, x2) -> (max acc1 x1, max acc2 x2)) xs


{-LECTURE 09-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-09.lhs

-- EXERCISE 01 =======================================================================

{-
  1.1.
  - Define a 'Date' structure with the appropriate fields.
  - Define a function that shows a date in the DD.MM.YYYY format (without
    leading zeroes).
    showDate :: Date -> String
-}

data Date = Date Int Int Int

showDate :: Date -> String
showDate (Date d m y) = show d ++ "." ++ show m ++ "." ++ show y

{-
  1.2.
  - Define a function
    translate :: Point -> Shape2 -> Shape2
    that translates a shape into the direction of vector (x,y).
-}

data Point = Point Double Double
  deriving Show

data Shape2 = Circle2 Point Double | Rectangle2 Point Point
  deriving Show

translate :: Point -> Shape2 -> Shape2
translate (Point x y) (Circle2 (Point x1 y1) r) = Circle2 (Point (x + x1) (y + y1)) r
translate (Point x y) (Rectangle2 (Point x1 y1) (Point x2 y2)) =
  Rectangle2 (Point (x1 + x) (y1 + y)) (Point (x2 + x) (y2 + y))

{-
  1.3.
  - Write a function 'inShape' that tests whether a point is contained within a
    given shape (or is on its border).
    inShape :: Shape2 -> Point -> Bool
  - Write a function 'inShapes' that tests if the point is within any shape from
    the list of shapes.
    inShapes :: [Shape2] -> Point -> Bool
-}

inShape :: Shape2 -> Point -> Bool
inShape (Circle2 (Point x1 y1) r) (Point x y)                = (x - x1) ^ 2 + (y - y1) ^ 2 <= r ^ 2
inShape (Rectangle2 (Point x1 y1) (Point x2 y2)) (Point x y) = x <= maximum [x1, x2] && x >= minimum [x1, x2] && y <= maximum [y1, y2] && y >= minimum [y1, y2]

inShapes :: [Shape2] -> Point -> Bool
inShapes shapes p = any (\a -> a) $ map (`inShape` p) shapes

{-
  1.4.
  - Define your type 'Vehicle' that can be a 'Car', 'Truck',
    'Motorcycle', or 'Bicycle'. The first three store a name of the manufacturer
    (String) and horsepower (Double).
  - Write a function 'totalHorsepower' that adds up the horsepower of the
    vehicles, assuming that bicycle's horsepower is 0.2.
-}

data Vehicle = Car String Double | Truck String Double | Motorcycle String Double | Bicycle

totalHorsepower :: [Vehicle] -> Double
totalHorsepower [] = 0
totalHorsepower (Car _ hp:xs) = hp + totalHorsepower xs
totalHorsepower (Truck _ hp:xs) = hp + totalHorsepower xs
totalHorsepower (Motorcycle _ hp:xs) = hp + totalHorsepower xs
totalHorsepower (Bicycle:xs) = 0.2 + totalHorsepower xs


-- EXERCISE 02 =======================================================================

data Level = Bachelor | Master | PhD deriving (Show, Eq)

data Student = Student
  { firstName  :: String
  , lastName   :: String
  , studentId  :: String
  , level      :: Level
  , avgGrade   :: Double
  } deriving Show

{-
  2.1.
  - Define a function that increases the average grade of the student by 1.0,
    but not above 5.0.
    improveStudent :: Student -> Student
-}

improveStudent :: Student -> Student
improveStudent student = student {avgGrade = min 5 $ avgGrade student + 1}

{-
  2.2.
  - Write a function to compute the average grade of students for the different
    study levels.
    avgGradePerLevels :: [Student] -> (Double, Double, Double)
-}

avgGradePerLevels :: [Student] -> (Double, Double, Double)
avgGradePerLevels = computeAvg . sumAndCount
  where
    addAndCountIfLevel (val, c) student lvl = if level student == lvl then (val + avgGrade student, c + 1) else (val, c)
    sumAndCount = foldr (\student (b, m, p) ->
      (addAndCountIfLevel b student Bachelor,
      addAndCountIfLevel m student Master,
      addAndCountIfLevel p student PhD)) ((0, 0), (0, 0), (0, 0))
    computeAvg ((b, cb), (m, cm), (p, cp)) = (b / cb, m / cm, p / cp)

{-
  2.3.
  - Write a function that returns a list of matriculation numbers for a given
    study level, sorted by average grade in descending order.
    rankedStudents :: Level -> [Student] -> [String]
-}

rankedStudents :: Level -> [Student] -> [String]
rankedStudents lvl = map studentId .
  sortBy (\x1 x2 -> avgGrade x2 `compare` avgGrade x1) .
    filter (\x -> level x == lvl)

{-
  2.4.
  - Write a function
    addStudent :: Student -> [Student] -> [Student]
    that adds a student to a list of students. If a student with an identical
    matriculation number already exists in the list, the function should return an
    error.
-}

addStudent :: Student -> [Student] -> [Student]
addStudent y xs =
  let alreadyExists = (length $ filter (\x -> studentId x == studentId y) xs) /= 0 in
      if alreadyExists then error "Student already in the list" else (y:xs)

-- EXERCISE 03 =======================================================================

{-
  3.1.
  - Define your own parametrized type 'MyTriplet' that contains the values of
    three different types. Do this using a record.
  - Define a function
    toTriplet :: MyTriplet a b c -> (a, b, c)
    that converts a 'MyTriplet' value into an ordinary triplet.
-}

data MyTriplet a b c = MyTriplet a b c

toTriplet :: MyTriplet a b c -> (a, b, c)
toTriplet (MyTriplet a b c) = (a, b, c)

{-
  3.2.
  - Define a function (Employee - salary :: Maybe Double, name :: String) deriving Show
    totalSalaries :: [Employee] -> Double
    that sums the known salaries of employees (salaries that are not 'Nothing').
-}

data Employee = Employee
  { name   :: String
  , salary :: Maybe Double
  } deriving Show

totalSalaries :: [Employee] -> Double
totalSalaries []                               = 0
totalSalaries (Employee {salary = Just d}:xs)  = d + totalSalaries xs
totalSalaries (Employee {salary = Nothing}:xs) = totalSalaries xs

{-
  3.3.
  - Write a function 'addStudent2' that works like 'addStudent' from problem 2.4
    but returns a 'Maybe' type instead of an error.
    addStudent2 :: Student -> [Student] -> Maybe [Student]
  - Write 'addStudent3' that returns an 'Either'.
-}

addStudent2 :: Student -> [Student] -> Maybe [Student]
addStudent2 y xs =
  let alreadyExists = (length $ filter (\x -> studentId x == studentId y) xs) /= 0 in
      if alreadyExists then Nothing else Just (y:xs)

addStudent3 :: Student -> [Student] -> Either String [Student]
addStudent3 y xs =
  let alreadyExists = (length $ filter (\x -> studentId x == studentId y) xs) /= 0 in
      if alreadyExists then Left "Student already in the list" else Right (y:xs)
