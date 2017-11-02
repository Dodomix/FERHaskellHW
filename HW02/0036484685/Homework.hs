module Homework where
--
import Data.List
import Data.Char
--

-- Task 01
toRNA :: String -> String
toRNA []     = []
toRNA (x:xs) = (nucleotideToRNA $ toUpper x) : toRNA xs

nucleotideToRNA :: Char -> Char
nucleotideToRNA 'G' = 'C'
nucleotideToRNA 'C' = 'G'
nucleotideToRNA 'T' = 'A'
nucleotideToRNA 'A' = 'U'
nucleotideToRNA  x  = error $ "Nucleotide " ++ show x ++ " doesn't exist"

-- Task 02

isOperationNegative :: [Int] -> Bool
isOperationNegative xs = odd $ length [x | x <- xs, x < 0] -- counts negative numbers and checks if the count is odd

multiply :: Int -> Int -> Int
multiply a b = if isOperationNegative [a, b] -- determines sign
                  then 0 - multiplyRecursive 0 smaller larger -- negative
                  else multiplyRecursive 0 smaller larger -- positive
                    where
                      absA                               = abs a
                      absB                               = abs b
                      smaller                            = min absA absB -- high quality optimization
                      larger                             = max absA absB
                      multiplyRecursive n 0 _            = n
                      multiplyRecursive n _ 0            = n
                      multiplyRecursive n smaller larger =
                            multiplyRecursive (n + larger) (smaller - 1) larger

divide :: Int -> Int -> Int
divide a b = if isOperationNegative [a, b]
            then 0 - divideRecursive 0 absA absB
            else divideRecursive 0 absA absB
              where absA = abs a
                    absB = abs b
                    divideRecursive _ _ 0 = error "Division by zero"
                    divideRecursive n a b = if a < 0
                                               then n - 1
                                               else divideRecursive (n + 1) (a - b) b

greatestCD :: Int -> Int -> Int
greatestCD a 0 = a
greatestCD a b = greatestCD b (a `mod` b)

-- Task 03
numberToWords :: Int -> String
numberToWords 0 = "zero"
numberToWords a = trim $ numberListToWords $ digits a -- need to trim because for example million creates a trailing whitespace

trim :: String -> String
trim xs = reverse $ recursiveTrim $ reverse xs -- trims trailing whitespace
  where
    recursiveTrim (' ':xs) = recursiveTrim xs
    recursiveTrim xs       = xs

digits :: Int -> [Int]
digits a = if a < 10 then [a] else digits (a `div` 10) ++ [a `mod` 10]

numberListToWords :: [Int] -> String
numberListToWords []           = ""
numberListToWords (0:xs)       = numberListToWords xs -- this function outputs 0 as ""
numberListToWords [1]          = "one "
numberListToWords [2]          = "two "
numberListToWords [3]          = "three "
numberListToWords [4]          = "four "
numberListToWords [5]          = "five "
numberListToWords [6]          = "six "
numberListToWords [7]          = "seven "
numberListToWords [8]          = "eight "
numberListToWords [9]          = "nine "
numberListToWords [1, 0]       = "ten "
numberListToWords [1, 1]       = "eleven "
numberListToWords [1, 2]       = "twelve "
numberListToWords [1, 3]       = "thirteen "
numberListToWords [1, 5]       = "fifteen "
numberListToWords [1, a0]      = numberListToWords [a0] ++ "teen "
numberListToWords [2, a0]      = "twenty-" ++ numberListToWords [a0]
numberListToWords [3, a0]      = "thirty-" ++ numberListToWords [a0]
numberListToWords [4, a0]      = "forty-" ++ numberListToWords [a0]
numberListToWords [5, a0]      = "fifty-" ++ numberListToWords [a0]
numberListToWords [8, a0]      = "eighty-" ++ numberListToWords [a0]
numberListToWords [a1, a0]     = numberListToWords [a1] ++ "ty-" ++ numberListToWords [a0] -- two digit number
numberListToWords [a2, a1, a0] = numberListToWords [a2] ++ "hundred " ++ numberListToWords [a1, a0] -- three digit number
numberListToWords xs           = -- for numbers with more than 3 digits
  if len >= 7
    then numberListToWords numberHead ++ "million " ++ numberListToWords numberTail
    else if len >= 4
      then numberListToWords numberHead ++ "thousand " ++ numberListToWords numberTail
      else ""
        where
          len = length xs
          splitLocation = if len >= 7 then len - 6 else len - 3
          parts = splitAt splitLocation xs
          numberHead = fst parts
          numberTail = snd parts

-- Task 04
undefined' :: a
undefined' = error "undefined'"
