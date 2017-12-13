module Homework where
--
import Data.List
--

-- Task 01

{-
  Inspect type signatures of functions that you are supposed to implement
  and and from that information try to think of how Robot and Bearing
  data types should look like.
-}

data Robot = Robot Bearing (Integer, Integer) deriving Show
data Bearing = North | East | South | West deriving Show

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot bearing (x, y) = Robot bearing (x, y)

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ p) = p

{- It is MANDATORY to implement 'simulate' function in terms of fold -}
simulate :: Robot -> String -> Robot
simulate r instr = foldl executeCommand r instr
  where
    executeCommand (Robot b p) c
      | c == 'A' = mkRobot b $ advance b p
      | c == 'L' = (mkRobot $ turnLeft b) p
      | c == 'R' = (mkRobot $ turnRight b) p
      | otherwise = error "Instruction not recognized"

advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance East (x, y) = (x + 1, y)
advance North (x, y)  = (x, y + 1)
advance West (x, y) = (x - 1, y)
advance South (x, y)  = (x, y - 1)

turnLeft :: Bearing -> Bearing
turnLeft East  = North
turnLeft North = West
turnLeft West  = South
turnLeft South = East

turnRight :: Bearing -> Bearing
turnRight East  = South
turnRight North = East
turnRight West  = North
turnRight South = West

-- Task 02

data TriangleType = Equilateral | Isosceles | Scalene | Degenerate | Illegal deriving Show

triangleType :: (Ord a, Num a) => a -> a -> a -> TriangleType
triangleType a b c
  | a <= 0 || b <= 0 || c <= 0 || a + b < c || a + c < b || b + c < a = Illegal
  | a + b == c || a + c == b || b + c == a = Degenerate
  | a == b && b == c = Equilateral
  | a == b || b == c || a == c = Isosceles
  | otherwise = Scalene

-- Task 03

{- some convenient test examples -}
-- splitter " > " " > " => ["", ""]
-- splitter " > " "123 > " => ["123", ""]
-- splitter " > " "123 > 456 > 789" => ["123", "456", "789"]

{-
  you don't have to bother with splitting on an empty list e.g.:
  splitter "" "abcde" => ["a", "b", "c", "d", "e"]
-}

{- It is MANDATORY to implement 'splitter' function in terms of fold -}
splitter :: Eq a => [a] -> [a] -> [[a]]
splitter d = foldr match [[]]
  where
    match c (h:rest) = if d `isPrefixOf` (c:h)
                      then ([]:(drop $ length d - 1) h:rest)
                      else ((c:h):rest)

-- Task 04

{-
  For this task either write a solution to the problem or if you think
  solution doesn't exist explain why that is the case. Of corse, solution
  must use fold.
-}
