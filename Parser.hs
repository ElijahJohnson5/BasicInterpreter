module Parser
  (parseBasicFile, parseTest, liftUnary, liftBoolUnary, liftRela, liftOp, Statement(..), Expression(..)) where

import Prelude hiding (lines)
import Parselib hiding (space, int)
import Data.Char
import Control.Monad
import Data.Array.IO

data Statement = Dim [Expression]
  | End
  | For Expression Expression Expression Expression
  | Goto Int
  | GoSub Int
  | If Expression Int
  | Input String [Expression]
  | Let Expression Expression
  | Next [Expression]
  | On Expression [Expression]
  | Print [Expression]
  | Rem String
  | Return
  | Assignment Expression Expression 
  | NoOp deriving Show

data Expression = BinaryOp String (Expression -> Expression -> Expression) Expression Expression
  | Not Expression
  | Negate Expression
  | Id Char | Function String Expression 
  | IntConst { intConst :: Int } | StringConst String 
  | FloatConst { floatConst :: Float } 
  | BoolConst { boolConst :: Bool } | Array Expression [Expression] 
  | SemiPrint Expression | CommaPrint Expression | NewLineExpression Expression
  | Array1D { array1d :: (IOArray Int Expression) } 
  | Array2D { array2d :: (IOArray (Int, Int) Expression) }

instance Show Expression where
  show (BinaryOp a _ b c) = "BinaryOp: " ++ show b ++ a ++ show c
  show (Not a) = "NOT " ++ show a
  show (Negate a) = "-" ++ show a
  show (Id a) = show a
  show (Function a b) = a ++ "(" ++ show b ++ ")"
  show (IntConst a) = show a
  show (StringConst a) = a
  show (FloatConst a) = show a
  show (BoolConst a) = show a
  show (Array a b) = show a ++ "(" ++ show b ++ ")"
  show (SemiPrint a) = show a
  show (CommaPrint a) = show a
  show (NewLineExpression a) = "NewLineExpression: " ++ show a

liftRela op (IntConst a) (IntConst b) = BoolConst $ (fromIntegral a) `op` (fromIntegral b)
liftRela op (FloatConst a) (FloatConst b) = BoolConst $ a `op` b
liftRela op (FloatConst a) (IntConst b) = BoolConst $ a `op` (fromIntegral b)
liftRela op (IntConst a) (FloatConst b) = BoolConst $ (fromIntegral a) `op` b
liftRela op a b = error ("A: " ++ show a ++ " B: " ++ show b)

liftOp op (IntConst a) (IntConst b) = IntConst . floor $ (fromIntegral a) `op` (fromIntegral b)
liftOp op (FloatConst a) (FloatConst b) = FloatConst $ a `op` b
liftOp op (FloatConst a) (IntConst b) = FloatConst $ a `op` (fromIntegral b)
liftOp op (IntConst a) (FloatConst b) = FloatConst $ (fromIntegral a) `op` b

liftOp' op (IntConst a) (IntConst b) = FloatConst $ (fromIntegral a) `op` (fromIntegral b)
liftOp' op x y = liftOp op x y

liftAndOr op (BoolConst a) (BoolConst b) = BoolConst $ a `op` b

liftBoolUnary op (BoolConst a) = BoolConst $ op a
liftUnary op (IntConst a) = IntConst . floor $ op (fromIntegral a)
liftUnary op (FloatConst a) = FloatConst $ op a

option res p = do { a <- p; return a } +++ return res

oneOf [] = mzero
oneOf (x:xs) = do { a <- char x; return a } +++ oneOf xs

space = many (oneOf " \t")

bString :: Parser String
bString = do { space; char '\"'; a <- many (sat (/= '"')); char '\"'; space; return a; }

int :: Parser Int
int = do {char '-'; n <- natural; return (-n)} +++ natural

bInt :: Parser Int
bInt = do { a <- int; space; return a }

bId :: Parser Char
bId = do { a <- sat (isLetter); space; return a }

symbol s = do { string s; space; }

parseString = do { a <- bString; return (StringConst a) }

parseId = do { a <- bId; return (Id a) }

parseInt = do { a <- bInt; return (IntConst a) }

expression = chainl1 andExpression op
  where op = do { symbol "OR"; return $ BinaryOp "OR"  (liftAndOr (||)) }

andExpression = chainl1 notExpression op
  where op = do { symbol "AND"; return $ BinaryOp "AND" (liftAndOr (&&)) }

notExpression = do { symbol "NOT"; a <- compareExpression; space; return (Not a) } 
  +++ compareExpression

compareExpression = chainl1 addExpression op
  where op = do { symbol "<>"; return $ BinaryOp "<>" (liftRela (/=)) } 
            +++ do { symbol ">="; return $ BinaryOp ">=" (liftRela (>=)) } 
            +++ do { symbol "<="; return $ BinaryOp "<=" (liftRela (<=)) }
            +++ do { symbol ">"; return $ BinaryOp ">" (liftRela (>)) } 
            +++ do { symbol "<"; return $ BinaryOp "<" (liftRela (<)) } 
            +++ do { symbol "="; return $ BinaryOp "=" (liftRela (==)) } 

addExpression = chainl1 multExpression op
  where op = do { symbol "+"; return $ BinaryOp "+" (liftOp (+))  } +++ do { symbol "-"; return $ BinaryOp "-" (liftOp (-)) }

multExpression = chainl1 negateExpression op
  where op = do { symbol "*"; return $ BinaryOp "*" (liftOp (*)) } +++ do { symbol "/"; return $ BinaryOp "/" (liftOp (/)) }

negateExpression = do { char '-'; a <- powerExpression; space; return (Negate a) } 
  +++ powerExpression

powerExpression = chainl1 value op
  where op = do { symbol "^"; return $ BinaryOp "^" (liftOp' (**)) }

parenExpression = do { symbol "("; a <- expression; space; symbol ")"; return a }

value = parenExpression +++ function +++ variable +++ constant

variable = arr +++ parseId

expressionList = do { a <- expression; space; symbol ","; b <- expressionList; space; return (a : b); } +++ do { a <- expression; return [a] }

arr = do 
  a <- parseId 
  space 
  symbol "(" 
  b <- expressionList
  space 
  symbol ")"
  return (Array a b)

arrList = sepby1 arr (space >> symbol ",")

idList = sepby1 parseId (space >> symbol ",") 

intList = sepby1 parseInt (space >> symbol ",")

printList = do { a <- expression; symbol ","; b <- printList; return ((CommaPrint a) : b) } +++ do { a <- expression; symbol ";"; b <- printList; return ((SemiPrint a) : b) } +++ do { a <- expression; space; return [(NewLineExpression a)] } +++ do { return [] }

function = do { symbol "INT"; a <- parenExpression; return (Function "INT" a) } 
  +++ do { symbol "RND"; a <- parenExpression; return (Function "RND" a) } +++ do { symbol "TAB"; a <- parenExpression; return (Function "TAB" a)}

constant = parseInt +++ parseString

parseDim = do { symbol "DIM"; a <- arrList; return (Dim a); }

parseEnd = do { symbol "END"; space; return End }

parseFor = do { symbol "FOR"; a <- parseId; symbol "="; b <- expression; symbol "TO"; c <- expression; d <- parseStep;  return (For a b c d) }

parseStep = do { symbol "STEP"; expression } +++ return (IntConst 1)

parseGoto = do { symbol "GOTO"; a <- bInt; return (Goto a) }

parseGoSub = do { symbol "GOSUB"; a <- bInt; return (GoSub a) }

parseIf = do { symbol "IF"; a <- expression; symbol "THEN"; b <- bInt; return (If a b) }

parseInput = do { symbol "INPUT"; a <- option "" bString; space; option ' ' (char ';'); space; b <- idList; return (Input a b) }

parseLet = do { symbol "LET"; a <- variable; symbol "="; space; b <- expression; return (Let a b) }

parseNext = do { symbol "NEXT"; a <- idList; return (Next a) }

parseOn = do { symbol "ON"; a <- expression; symbol "GOTO"; b <- intList; return (On a b) }

parsePrint = do { symbol "PRINT"; a <- printList; return (Print a) } +++ do { symbol "PRINT"; return $ Print [] }

parseRem = do { symbol "REM"; a <- many (sat isPrint); return (Rem a) }

parseReturn = do { symbol "RETURN"; return Return }

parseAssignment = do { a <- variable; space; symbol "="; b <- expression; return (Assignment a b) }

statement = parseDim +++ parseEnd +++ parseFor +++ parseGoto +++ parseGoSub +++ parseIf +++ parseInput +++ parseLet +++ parseNext +++ parseOn +++ parsePrint +++ parseRem +++ parseReturn +++ parseAssignment

statements = do { a <- statement; symbol ":"; b <- statements; return (a : b); } +++ do { a <- statement; return [a] } 

line = do { n <- int; space; ss <- statements; space; option "" (symbol "\n"); return (n,ss) }

lines = many1 line

parseBasicFile s = do
  fs <- readFile s
  return (fst $ head $ parse lines fs)

parseTest s = do
  fs <- readFile s
  putStrLn fs
  return (fst $ head $ parse lines fs)