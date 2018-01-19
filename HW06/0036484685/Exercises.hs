{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--
import Data.Set hiding (foldr, map, null, filter)
import Data.List
import System.Random
import System.IO
import System.Directory
import Control.Monad
import Control.Exception
import Data.Char

{-
    Here you should provide your solutions to in-class exercises.

    Make sure that ALL FUNCTIONS (including exXXX) have correct TYPE SIGNATURES.

    You should include solutions from following lectures :
    http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-10.lhs
    http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-11.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 10-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-10.lhs

-- EXERCISE 01 =======================================================================
{-
  1.2.
  - Define a function
    parentCheck :: Person2 -> Bool
    that checks whether the given person is one of the children of its parents.
-}
data Sex = Male | Female deriving (Show,Read,Ord,Eq)

data Person2 = Person2 {
  personId2 :: String,
  forename2 :: String,
  surname2  :: String,
  sex2      :: Sex,   --- data Sex = Male | Female deriving (Show,Read,Eq,Ord)
  mother2   :: Maybe Person2,
  father2   :: Maybe Person2,
  partner2  :: Maybe Person2,
  children2 :: [Person2] } deriving (Show,Read,Eq,Ord)

parentCheck :: Person2 -> Bool
parentCheck p = let isDescendent parent =
                      any (\c -> personId2 c == personId2 p) $ children2 parent in
                    case mother2 p of
                      Just m -> isDescendent m
                      Nothing -> True
                    &&
                    case father2 p of
                      Just f -> isDescendent f
                      Nothing -> True

{-
  1.3.
  - Define a function
    sister :: Person2 -> Maybe Person2
    that returns the sister of a person, if such exists.
-}

sister :: Person2 -> Maybe Person2
sister p = case sisterM of
             Just s -> s
             Nothing -> case sisterF of
                          Just s -> s
                          Nothing -> Nothing
  where
    sisterImpl parent =
      foldr (\person acc ->
        if sex2 person == Female && personId2 p /= personId2 person
           then Just person else acc)
      Nothing $ children2 parent
    sisterM = fmap sisterImpl $ mother2 p
    sisterF = fmap sisterImpl $ father2 p

{-
  1.4.
  - Define a function that returns all descendants of a person.
    descendant :: Person2 -> [Person2]
-}

descendant :: Person2 -> [Person2]
descendant x = children2 x ++ (concat $ map (\c -> descendant c) $ children2 x)

-- EXERCISE 02 =======================================================================
{-
  2.1.
  - Define
    listHead :: MyList a -> Maybe a
-}

data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord,Eq)

infixr 5 -+-
(-+-) = Cons

listHead :: MyList a -> Maybe a
listHead Empty = Nothing
listHead (Cons x _) = Just x

{-
  2.2.
  - Define a function that works like 'map' but works on a 'MyList' type:
    listMap :: (a -> b) -> MyList a -> MyList b
-}

listMap :: (a -> b) -> MyList a -> MyList b
listMap _ Empty = Empty
listMap f (Cons x xs) = f x -+- listMap f xs

-- EXERCISE 03 =======================================================================
{-
  3.1.
  - Define a function
    treeMax :: Ord a => Tree a -> a
    that finds the maximum element in a tree. Return an error if the tree is
    empty.
-}

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show,Eq)

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r)
  | x < y     = Node y (treeInsert x l) r
  | x > y     = Node y l (treeInsert x r)
  | otherwise = t

treeMax :: Ord a => Tree a -> a
treeMax Null                = error "Tree is empty"
treeMax (Node x left right) = maximumBy compare [x, treeMax left, treeMax right]

{-
  3.2.
  - Define a function
    treeToList :: Ord a => Tree a -> [a]
    that will collect in a list all elements from inner nodes of a tree by doing
    an in-order (left-root-right) traversal.
-}

treeToList :: Ord a => Tree a -> [a]
treeToList Null                = []
treeToList (Node x left right) = treeToList left ++ [x] ++ treeToList right

{-
  3.3.
  - Define a function to prune the tree at a given level (root has level 0).
    levelCut :: Int -> Tree a -> Tree a
-}

levelCut :: Int -> Tree a -> Tree a
levelCut 0 Null                = Null
levelCut 0 (Node x _ _)        = Node x Null Null
levelCut n (Node x left right) = Node x (levelCut (n-1) left) (levelCut (n-1) right)

-- EXERCISE 04 =======================================================================
{-
  4.1.
  - Define a function that converts a list into a sorted tree.
    listToTree :: Ord a => [a] -> Tree a
-}

listToTree :: Ord a => [a] -> Tree a
listToTree = foldr treeInsert Null

{-
  4.2.
  - Using 'listToTree' and 'treeToList' defined previously, define these two
    functions, define:
    sortAndNub :: Ord a => [a] -> [a]
-}

sortAndNub :: Ord a => [a] -> [a]
sortAndNub = treeToList . listToTree

-- EXERCISE 05 =======================================================================
{-
  5.1.
  - Define an 'Eq' instance for the 'Weekday' type that works like (==), except
    that two Fridays are never identical.
-}

data Weekday =
  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show,Enum)

instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False

{-
  5.2.
  - Define 'Person' as an instance of 'Show' type class so that instead of the
    values of partners and children only the respective person names are shown,
    which will enable the print out of an infinite structure of this type.
-}

data Person = Person
  { idNumber :: String
  , forename :: String
  , surname  :: String
  , sex      :: Sex
  , age      :: Int
  , partner  :: Maybe Person
  , children :: [Person]
  } deriving (Read,Eq,Ord)

instance Show Person where
  show p = "Person {idNumber=\"" ++ idNumber p ++
    "\",forename=\"" ++ forename p ++
      "\",surname=\"" ++ surname p ++
        "\",sex=" ++ (show $ sex p) ++
          ",age=" ++ (show $ age p) ++
            ",partner=" ++ show ((fmap printPerson $ partner p)) ++
              ",children=" ++ show (map printPerson $ children p) ++ "}"
    where printPerson p = forename p ++ " " ++ surname p

{-LECTURE 11-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-11.lhs

-- EXERCISE 01 =======================================================================

{- DON'T FORGET TO WRITE TYPE SIGNATURES-}

{-
  1.1.
  - Define a 'main' function that reads in two strings and prints them out
    concatenated and reversed.
-}

main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  putStrLn $ reverse $ s1 ++ s2

{-
  1.2.
  - Write a function 'threeNumbers' that reads in three numbers and prints out
    their sum.
-}

threeNumbers :: IO ()
threeNumbers = do
  s1 <- readLn::IO Int
  s2 <- readLn::IO Int
  s3 <- readLn::IO Int
  putStrLn $ show $ s1 + s2 + s3

-- EXERCISE 02 =======================================================================
{-
  2.1.
  - Define a function 'threeStrings' that reads in three strings and outputs them
    to the screen as one string, while it returns its total length.
    treeStrings :: IO Int
-}

treeStrings :: IO Int
treeStrings = do
  s1 <- getLine
  s2 <- getLine
  s3 <- getLine
  let s = s1 ++ s2 ++ s3
  putStrLn s
  return $ length s

{-
  2.2.
  - Define a function 'askNumber9' that reads in a number and returns that number
    converted into an 'Int'. Input should be repeated until the user enters a
    number (a string containing only digits).
      askNumber9 :: IO Int
-}

askNumber9 :: IO Int
askNumber9 = do
  putStrLn "Enter your lucky number"
  number <- getLine
  if number == "" then do
    putStr "No input! "
    askNumber9
  else return $ read number

{-
  2.3.
  - Define a function 'askUser m p' that returns an action that prints out 'm',
    reads in a string from the input, repeats the input until the input
    string satisfies the function 'p', and then returns the input string.
      askUser :: String -> (String -> Bool) -> IO String
  - Generalize this function to
      askUser' :: Read a => String -> (String -> Bool) -> IO a
-}

askUser :: String -> (String -> Bool) -> IO String
askUser m p = do
  putStrLn m
  s <- getLine
  if p s then return s else do
    s <- askUser m p
    return s

askUser' :: Read a => String -> (String -> Bool) -> IO a
askUser' m p = do
  putStrLn m
  s <- getLine
  if p s then return $ read s else do
    s <- askUser m p
    return $ read s

{-
  2.4.
  - Define a function that reads in strings until the user inputs an empty
    string, and then returns a list of strings received as input.
      inputStrings :: IO [String]
-}

inputStrings :: IO [String]
inputStrings = do
  x <- getLine
  if null x then return [] else do
    xs <- inputStrings
    return $ x:xs

-- EXERCISE 03 =======================================================================
{-
  3.1.
  - Define a function that reads in a number, then reads in that many
    strings, and finally prints these strings in reverse order.
-}

f1 :: IO ()
f1 = do
  n <- readLn
  s <- replicateM n getLine
  putStrLn $ concat $ reverse s

{-
  3.2.
  - Give recursive definitions for 'sequence' and 'sequence_'.
-}

sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (x:xs) = do
  notIO <- x
  rest <- sequence xs
  return $ notIO:rest

sequence_' :: [IO a] -> IO ()
sequence_' xs = do
  sequence' xs
  return ()

{-
  3.3.
  - Give a recursive definitions for 'mapM' and 'mapM_'.
-}

mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' f = sequence' . map f

mapM_' :: (a -> IO b) -> [a] -> IO ()
mapM_' f = sequence_' . map f

{-
  3.4.
  - Define a function that prints out the Pythagorean triplets whose all sides
    are <=100. Every triplet should be in a separate line.
-}

printTriplets :: IO ()
printTriplets = do
  mapM_ putStrLn
    ["(3, 4, 5)", "(5, 12, 13)", "(8, 15, 17)", "(7, 24, 25)",
    "(20, 21, 29)", "(12, 35, 37)", "(9, 40, 41)", "(28, 45, 53)",
    "(11, 60, 61)", "(33, 56, 65)", "(16, 63, 65)", "(48, 55, 73)",
    "(36, 77, 85)", "(13, 84, 85)", "(39, 80, 89)", "(65, 72, 97)"]

-- EXERCISE 04 =======================================================================
{-
  4.1.
  - Define a function that removes from standard input every second line and
    prints the result to standard output.
      filterOdd :: IO ()
-}

filterOdd :: IO ()
filterOdd = do
  s <- getContents
  print $ unlines $ map snd $ filter (\(n, _) -> even n) $ zip [0..] $ lines s

{-
  4.2.
  - Define a function that prefixes each line from standard input with a line
    number (number + space).
      numberLines :: IO ()
-}

numberLines :: IO ()
numberLines = do
  s <- getContents
  print $ unlines $ map (\(n, l) -> show n ++ " " ++ l) $  zip [1..] $ lines s

{- 4.3.
  - Define a function to remove from standard input all words from a given set of
    words.
      filterWords :: Set String -> IO ()
-}

filterWords :: Set String -> IO ()
filterWords xs = do
  s <- getContents
  print $ unwords $ filter (`notElem` xs) $ words s

-- EXERCISE 05 =======================================================================
{-
  5.1.
  - Define a function
    wc :: FilePath -> IO (Int, Int, Int)
    that counts the number of characters, words, and lines in a file.
-}

wc :: FilePath -> IO (Int, Int, Int)
wc f = do
  s <- readFile f
  return (length s, length $ words s, length $ lines s)

{-
  5.2.
  - Define a function
    copyLines :: [Int] -> FilePath -> FilePath -> IO ()
    that copies given lines from the first file into the second.
-}

copyLines :: [Int] -> FilePath -> FilePath -> IO ()
copyLines xs src dst = do
  s <- readFile src
  writeFile dst $ unlines $ map snd $ filter (\x -> fst x `elem` xs) $ zip [1..] $ lines s

-- EXERCISE 06 =======================================================================
{-
  6.1.
  - Define a function
      wordTypes :: FilePath -> IO Int
    to compute the number of distinct words in the given file.
-}

wordTypes :: FilePath -> IO Int
wordTypes f = do
  s <- readFile f
  return $ length $ fromList $ words s

{-
  6.2.
  - Define a function
      diff :: FilePath -> FilePath -> IO ()
    that takes two file names, compares their corresponding lines, and then
    outputs to standard output all lines in which the files differ. Lines should
    be printed one below the other, prefixed with "<" for the first and ">" for
    the second file.
-}

diff :: FilePath -> FilePath -> IO ()
diff f1 f2 = do
  s1 <- readFile f1
  s2 <- readFile f2
  let l1 = lines s1
  let l2 = lines s2
  let zipped = let len = max (length l1) (length l2)
                   l1' = l1 ++ (repeat "")
                   l2' = l2 ++ (repeat "")
               in take len $ zip l1' l2'
  mapM_ (\(l1, l2) -> if l1 /= l2 then do
                                  putStrLn $ "<" ++ l1
                                  putStrLn $ ">" ++ l2
                             else putStr "") zipped

{-
  6.3.
  - Define a function
      removeSpaces :: FilePath -> IO ()
    that removes trailing spaces from all lines in the given file.
    The function should change the original file.
-}

removeSpaces :: FilePath -> IO ()
removeSpaces f = do
  (ft, ht) <- openTempFile "" f
  s <- readFile f
  hPutStr ht . unlines . map trim $ lines s
  hClose ht
  renameFile ft f


trim :: String -> String
trim = reverse . dropWhile isSpace . reverse

-- EXERCISE 07 =======================================================================
{-
  7.1.
  - Define a function
      fileHead :: IO ()
    that prints the first 'n' lines from a file. The name of the file and the
    number of lines are specified at the command line, e.g.:
      filehead -5 input.txt
    If the number of lines is missing, default to 10. If file name is missing,
    read from the standard input. If the file doesn't exist, print an error
    message and exit with failure using 'exitFailure' from 'System.Exit'.
-}

fileHead :: IO ()
fileHead = undefined

{-
  7.2.
  - Define a function
      sortFiles :: IO ()
    that sorts lines from multiple files and prints them to standard output.
    File names are provided at the command line.
    "sortFiles file1.txt file2.txt file3.txt"
    If any of the files does not exist, print an error message.
-}

sortFiles :: IO ()
sortFiles = undefined

-- EXERCISE 08 =======================================================================
{-
  8.1.
  - Define your own implementation of
      randoms' :: (RandomGen g, Random a) => g -> [a]
-}

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' = undefined -- why doesn't the below definition work?
--randoms' g = (a:randoms' g')
--  where (a, g') = random g

{-
  8.2.
  - Define a function
      randomPositions :: Int -> Int -> Int -> Int -> IO [(Int,Int)]
    that returns a list of randomly generated integer coordinates from within a
    given interval.
      randomPositions 0 10 0 10 => [(2,1),(4,3),(7,7),...
-}

randomPositions :: Int -> Int -> Int -> Int -> IO [(Int,Int)]
randomPositions minX maxX minY maxY = do
  x <- getStdRandom (randomR (minX, maxX)) :: IO Int
  y <- getStdRandom (randomR (minY, maxY)) :: IO Int
  rest <- randomPositions minX maxX minY maxY
  return ((x, y):rest)
