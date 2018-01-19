module Homework where
--
import Data.List
import Data.Time.Clock ( UTCTime(..) )
import Data.Time.Calendar ( Day, gregorianMonthLength, fromGregorian )
import Data.Time.Format ( formatTime, defaultTimeLocale )
--

-- Task 01
data Pred = Val Bool | Not Pred | And Pred Pred | Or Pred Pred deriving Show

eval :: Pred -> Bool
eval (Val b)     = b
eval (Not p)     = not $ eval p
eval (And p1 p2) = eval p1 && eval p2
eval (Or p1 p2)  = eval p1 || eval p2

-- Task 02

dateFromDescription :: String -> Day
dateFromDescription = undefined

-- Task 03

data Tree a
  = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq, Show)

-- a)
treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter p Leaf             = Leaf
treeFilter p (Node x lst rst) = if p x then Node x (treeFilter p lst) (treeFilter p rst) else Leaf

-- b)
levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap = levelMapRecursive 0
  where levelMapRecursive n f Leaf = Leaf
        levelMapRecursive n f (Node x lst rst) =
          Node (f n x) (levelMapRecursive (n + 1) f lst) (levelMapRecursive (n + 1) f rst)

-- c)

isEqual :: Eq a => Tree a -> Tree a -> Bool
isEqual Leaf Leaf = True
isEqual (Node x1 lst1 rst1) (Node x2 lst2 rst2) = x1 == x2 && isEqual lst1 lst2 && isEqual rst1 rst2
isEqual _ _ = False

isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree x Leaf = x == Leaf
isSubtree x n@(Node a lst rst) = isEqual x n || isSubtree x lst || isSubtree x rst

-- Task 04
data Category = Category String [Category] deriving (Show)

instance Eq Category where
  Category x _ == Category y _ = x == y

splitter :: Eq a => [a] -> [a] -> [[a]]
splitter d = foldr match [[]]
  where
    match c (h:rest) = if d `isPrefixOf` (c:h)
                      then ([]:(drop $ length d - 1) h:rest)
                      else ((c:h):rest)

parseCategories :: [String] -> [Category]
parseCategories = combine . concat . map parseCategory

-- this function is used to recursively combine all categories with the same name
combine :: [Category] -> [Category]
combine = foldr
  (\x acc ->
    if x `elem` acc then map (\a ->
      if x == a then (Category (name x) (combine $ subcategories x ++ subcategories a))
      else a) acc
    else x:acc) []

name :: Category -> String
name (Category s _) = s

subcategories :: Category -> [Category]
subcategories (Category _ xs) = xs

parseCategory :: String -> [Category]
parseCategory = foldr (\s acc -> [Category s acc]) [] . splitter " > "

printCategories :: [Category] -> [String]
printCategories = nub . concat . map printCategory

printCategory :: Category -> [String]
printCategory (Category x cs) = x:(concat $ map (map (\c -> x ++ " > " ++ c) . printCategory) cs)

fromFile :: FilePath -> IO [Category]
fromFile f = do
  s <- readFile f
  return $ parseCategories $ lines s
