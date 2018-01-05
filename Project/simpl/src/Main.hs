module Main where
import Text.ParserCombinators.Parsec hiding (State)

-- Example usage in command line after installed
-- simpl
-- In <- 5
-- file
-- test1
-- END

main :: IO ()
main = do
  putStrLn "Please enter either \"file\" with file name in the next line or lines to parse, followed by END in the last line"
  s <- constructStatement Skip
  putStrLn $ show s
  putStrLn $ "Result in Out variable: " ++ (show $ (run empty s) "Out")

constructStatement :: Statement -> IO Statement
constructStatement s = do
  line <- getLine
  case line of
    "file" -> do
                putStrLn "Please enter file name"
                f <- getLine
                fileInput <- readFile f
                parsed <- parseStatement fileInput
                constructStatement (Sequence s parsed)
    "END" -> return s
    otherwise -> do
                  parsed <- parseStatement line
                  constructStatement (Sequence s parsed)

parseStatement :: String -> IO Statement
parseStatement s = do
  case Main.parse s of
    Nothing -> do
      putStrLn "Could not parse statement"
      return Skip
    Just s1 -> return s1

data Expression
  = Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop
  = Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement
  = Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Part 01 -----------------------------------------

extend :: State -> String -> Int -> State
extend state x val = appendedState
  where appendedState s = if s == x then val else state s

empty :: State
empty = emptyState
  where emptyState _ = 0

-- Part 02 -----------------------------------------

evalE :: State -> Expression -> Int
evalE s (Var x)       = s x
evalE s (Val x)       = x
evalE s (Op e1 op e2) = opToFunction op (evalE s e1) (evalE s e2)

opToFunction :: Bop -> Int -> Int -> Int
opToFunction Plus   = (+)
opToFunction Minus  = (-)
opToFunction Times  = (*)
opToFunction Divide = div
opToFunction Gt     = \a b -> boolToInt $ a > b
opToFunction Ge     = \a b -> boolToInt $ a >= b
opToFunction Lt     = \a b -> boolToInt $ a < b
opToFunction Le     = \a b -> boolToInt $ a <= b
opToFunction Eql    = \a b -> boolToInt $ a == b

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

intToBool :: Int -> Bool
intToBool 0 = False
intToBool _ = True

-- Part 03 -----------------------------------------

data DietStatement
  = DAssign String Expression
  | DIf Expression DietStatement DietStatement
  | DWhile Expression DietStatement
  | DSequence DietStatement DietStatement
  | DSkip
  deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s e) = DAssign s e
desugar (Incr x) = DAssign x (Op (Var x) Plus (Val 1))
desugar (If e s1 s2) = DIf e (desugar s1) (desugar s2)
desugar (While e s) = DWhile e $ desugar s
desugar (For s1 e s2 s3) = DSequence (desugar s1) (DWhile e $ DSequence (desugar s3) (desugar s2))
desugar (Sequence s1 s2) = DSequence (desugar s1) (desugar s2)
desugar Skip = DSkip


-- Part 04 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple s (DAssign x e) = extend s x $ evalE s e
evalSimple s (DIf e s1 s2) = if intToBool $ evalE s e then evalSimple s s1 else evalSimple s s2
evalSimple s w@(DWhile e s1) = if intToBool $ evalE s e then evalSimple newS w else s
  where newS = evalSimple s s1
evalSimple s (DSequence s1 s2) = evalSimple (evalSimple s s1) s2
evalSimple s DSkip = s

run :: State -> Statement -> State
run s = evalSimple s . desugar

-- Part 05 -----------------------------------------

eol :: Parser String
eol = choice [string "\n", string ";\n"]

integer = read <$> (minus <|> number)
  where minus  = (:) <$> char '-' <*> number
        number = many1 digit

word :: Parser String
word = many1 $ choice [letter, digit]

skipEmpty :: Parser ()
skipEmpty = optional $ many1 $ choice [string " ", string "\n"]

parseMath :: Parser Expression
parseMath = do
  skipEmpty
  v1 <- choice [try parseLiteral, try parseVariable]
  skipEmpty
  op <- choice $ map (try . string) ["+", "-", "*", "/", ">=", "<=", ">", "<", "=="]
  skipEmpty
  v2 <- choice [try parseMath, try parseLiteral, try parseVariable]
  let op' = case op of
              "+" -> Op v1 Plus v2
              "-" -> Op v1 Minus v2
              "*" -> Op v1 Times v2
              "/" -> Op v1 Divide v2
              ">" -> Op v1 Gt v2
              ">=" -> Op v1 Ge v2
              "<" -> Op v1 Lt v2
              "<=" -> Op v1 Le v2
              "==" -> Op v1 Eql v2
  return op'

parseInc :: Parser Statement
parseInc = do
  skipEmpty
  v1 <- word
  string "++"
  return $ Incr v1

parseLiteral :: Parser Expression
parseLiteral = do
  v1 <- try integer
  return $ Val v1

parseVariable :: Parser Expression
parseVariable = do
  v1 <- try word
  return $ Var v1

parseAssign :: Parser Statement
parseAssign = do
  skipEmpty
  var <- word
  skipEmpty
  string ":="
  skipEmpty
  exp <- choice [try parseMath, try parseLiteral, try parseVariable]
  return $ Assign var exp

parseIf :: Parser Statement
parseIf = do
  skipEmpty
  string "if"
  skipEmpty
  char '('
  cond <- choice [try parseMath, try parseLiteral, try parseVariable]
  char ')'
  skipEmpty
  string "{"
  eol
  ifCode <- manyTill line' (try endOfStatement)
  skipEmpty
  string "else"
  skipEmpty
  string "{"
  eol
  elseCode <- manyTill line' (try endOfStatement)
  return $ If cond (slist ifCode) (slist elseCode)

parseWhile :: Parser Statement
parseWhile = do
  skipEmpty
  string "while"
  skipEmpty
  char '('
  cond <- choice [try parseMath, try parseLiteral, try parseVariable]
  char ')'
  skipEmpty
  string "{"
  eol
  code <- manyTill line' (try endOfStatement)
  skipEmpty
  return $ While cond (slist code)

parseFor :: Parser Statement
parseFor = do
  skipEmpty
  string "for"
  skipEmpty
  char '('
  init <- parseAssign
  char ';'
  cond <- parseMath
  char ';'
  rep <- choice [try parseAssign, try parseInc]
  char ')'
  skipEmpty
  string "{"
  eol
  code <- manyTill line' (try endOfStatement)
  skipEmpty
  return $ For init cond rep (slist code)

endOfStatement :: Parser String
endOfStatement = do
  skipEmpty
  string "}"

line' :: Parser Statement
line' = do
  l <- choice [try parseIf, try parseWhile, try parseFor, try parseAssign, try parseInc]
  optional eol
  optional newline
  return l

mainParser :: Parser Statement
mainParser = do
  lines <- manyTill line' (eof)
  return $ slist lines

parse :: String -> Maybe Statement
parse text = case Text.ParserCombinators.Parsec.parse mainParser "" text of
  (Left error) -> Nothing
  (Right s) -> Just s

parseFile :: String -> IO ()
parseFile f = do
  fileInput <- readFile f
  case Text.ParserCombinators.Parsec.parse mainParser "" fileInput of
    Left err -> putStrLn $ show err
    Right st -> putStrLn $ show st

-- Programs ----------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input
  for (Out := 1; In > 0; In := In - 1) {
      Out := In * Out
  }
-}

factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{-
  Calculate the floor of the square root of the input
  B := 0;
  while (A >= B * B) {
      B++
  };
  B := B - 1
-}

squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{-
  Calculate the nth Fibonacci number

  F0 := 1;
  F1 := 1;

  if (In == 0) {
      Out := F0
  } else {
      if (In == 1) {
          Out := F1
      } else {
          for (C := 2; C <= In; C++) {
              T  := F0 + F1;
              F0 := F1;
              F1 := T;
              Out := T
          }
      }
  }

-}

fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                    ]
