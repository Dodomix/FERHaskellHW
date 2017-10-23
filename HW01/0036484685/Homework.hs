module Homework where
--
import Data.List
import Data.Char
--

-- Task 01
isLeapYear :: Int -> Bool
isLeapYear year = and [year `mod` 4 == 0, or [year `mod` 100 /= 0, year `mod` 400 == 0]] -- a leap year is divisible by 4 and (not divisible by 100 or divisible by 400)

leapList :: [Int]
leapList = [year | year <- [1996..2017], isLeapYear year]

-- Task 02
evaluate :: Double -> [Double] -> Double
evaluate x a = sum [fst el * snd el | el <- zip [x^i | i <- [0..]] a] -- evaluates a polynomial

factorial :: Double -> Double
factorial n = product [1..n]

maclaurin :: [Double]
maclaurin = [1 / factorial i | i <- [0..]]

exp' :: Double -> Double
exp' x = evaluate x $ take 170 maclaurin

-- Task 03
findItem :: [(String, a)] -> String -> [(String, a)]
findItem keyValueList key = [item | item <- keyValueList, fst item == key]

contains :: [(String, a)] -> String -> Bool
contains keyValueList key = not $ null $ findItem keyValueList key

lookup :: [(String, a)] -> String -> a
lookup keyValueList key
  | contains keyValueList key = snd $ findItem keyValueList key !! 0
  | otherwise = error "Cannot lookup a value with a nonexistant key"

insert :: [(String, a)] -> (String, a) -> [(String, a)]
insert keyValueList element
  | contains keyValueList $ fst element = keyValueList -- do nothing if element with the same key exists
  | otherwise = element : keyValueList -- append the element if it does not exist

remove :: [(String, a)] -> String -> [(String, a)]
remove keyValueList key = [element | element <- keyValueList, fst element /= key]

update :: [(String, a)] -> String -> a -> [(String, a)]
update keyValueList key value = [if fst element == key then (key, value) else element | element <- keyValueList]

-- Task 04
lowerCaseLetters :: [Char] -> [Char]
lowerCaseLetters s = [toLower c | c <- s]

lowerCaseWords :: String -> [String]
lowerCaseWords s = words $ lowerCaseLetters s -- words of a string converted to lowercase

wordsUnion :: String -> String -> [String]
wordsUnion s1 s2 = nub $ lowerCaseWords s1 ++ lowerCaseWords s2 -- unique list of all words in two strings

constructWordsVector :: String -> [String] -> [Double]
constructWordsVector s allWords = [wordCount word $ lowerCaseWords s | word <- allWords] -- vector of word counts of allWords in a string

wordCount :: String -> [String] -> Double
wordCount s words = sum [if s == word then 1.0 else 0.0 | word <- words] -- count of word s in a list of words

dotProduct :: [Double] -> [Double] -> Double
dotProduct v1 v2 = sum [fst combined * snd combined | combined <- zip v1 v2]

vectorDistance :: [Double] -> Double
vectorDistance v = sqrt $ sum [el^2 | el <- v] -- absolute value of a vector

computeCosineSimilarity :: [Double] -> [Double] -> Double
computeCosineSimilarity v1 v2 = dotProduct v1 v2 / (vectorDistance v1 * vectorDistance v2) -- https://en.wikipedia.org/wiki/Cosine_similarity#Definition

cosineSimilarity :: String -> String -> Double
cosineSimilarity s1 s2 = computeCosineSimilarity (constructWordsVector s1 $ wordsUnion s1 s2) (constructWordsVector s2 $ wordsUnion s1 s2)

