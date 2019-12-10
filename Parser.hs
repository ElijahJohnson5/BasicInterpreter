module Parser
  (parseBasicFile, parseTest, liftUnary, liftBoolUnary, liftRela, liftOp, liftStringOp, liftRelaString, Statement(..), Expression(..)) where

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
  | IfElse Expression Int Int
  | Input String [Expression]
  | Let Expression Expression
  | Next [Expression]
  | On Expression [Expression]
  | Print [Expression]
  | Rem String
  | Return
  | Assignment Expression Expression 
  | NoOp deriving Show

data Expression = BinaryOp String (Expression -> Expression -> Expression) (Expression -> Expression -> Expression) Expression Expression
  | Not Expression
  | Negate Expression
  | Id String | Function String Expression 
  | IntConst { intConst :: Int } | StringConst String 
  | FloatConst { floatConst :: Float } 
  | BoolConst { boolConst :: Bool } | Array Expression [Expression] 
  | SemiPrint Expression | CommaPrint Expression | NewLineExpression Expression
  | Array1D { array1d :: (IOArray Int Expression) } 
  | Array2D { array2d :: (IOArray (Int, Int) Expression) }

instance Show Expression where
  show (BinaryOp a _ _ b c) = "BinaryOp: " ++ show b ++ a ++ show c
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

liftStringOp op (StringConst a) (IntConst b) = StringConst $ a `op` (show b)
liftStringOp op (StringConst a) (StringConst b) = StringConst $ a `op` b
liftStringOp op (StringConst a) (FloatConst b) = StringConst $ a `op` (show b)
liftStringOp op (IntConst a) (StringConst b) = StringConst $ (show a) `op` b
liftStringOp op (FloatConst a) (StringConst b) = StringConst $ (show a) `op` b

liftRelaString op (StringConst a) (StringConst b) = BoolConst $ a `op` b

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

bId :: Parser String
bId = do { a <- sat (isAlpha); as <- (many alphanum); char '$'; space; return ('$':a:as) } +++ do { a <- sat (isAlpha); as <- (many alphanum); space; return (a:as) } 

symbol s = do { string s; space; }

caseInsensitiveChar c = char (toLower c) +++ char (toUpper c)

caseInsensitiveString "" = return ""
caseInsensitiveString (c:cs) = do { caseInsensitiveChar c; caseInsensitiveString cs; return (c:cs) } 

caseInsensitiveSymbol s = do { caseInsensitiveString s; space; }

parseString = do { a <- bString; return (StringConst a) }

parseId = do { a <- bId; return (Id a) }

parseInt = do { a <- bInt; return (IntConst a) }

expression = chainl1 andExpression op
  where op = do { symbol "OR"; return $ BinaryOp "OR"  (liftAndOr (||)) ((liftAndOr (||))) }

andExpression = chainl1 notExpression op
  where op = do { symbol "AND"; return $ BinaryOp "AND" (liftAndOr (&&)) (liftAndOr (&&)) }

notExpression = do { symbol "NOT"; a <- compareExpression; space; return (Not a) } 
  +++ compareExpression

compareExpression = chainl1 addExpression op
  where op = do { symbol "<>"; return $ BinaryOp "<>" (liftRela (/=)) (liftRelaString (/=)) } 
            +++ do { symbol ">="; return $ BinaryOp ">=" (liftRela (>=)) (liftRelaString (>=)) } 
            +++ do { symbol "<="; return $ BinaryOp "<=" (liftRela (<=)) (liftRelaString (<=)) }
            +++ do { symbol ">"; return $ BinaryOp ">" (liftRela (>)) (liftRelaString (>)) } 
            +++ do { symbol "<"; return $ BinaryOp "<" (liftRela (<)) (liftRelaString (<)) } 
            +++ do { symbol "="; return $ BinaryOp "=" (liftRela (==)) (liftRelaString (==)) } 

addExpression = chainl1 multExpression op
  where op = do { symbol "+"; return $ BinaryOp "+" (liftOp (+)) (liftStringOp (++))  } 
            +++ do { symbol "-"; return $ BinaryOp "-" (liftOp (-)) (liftOp (-)) }

multExpression = chainl1 negateExpression op
  where op = do { symbol "*"; return $ BinaryOp "*" (liftOp (*)) (liftOp (*)) } 
            +++ do { symbol "/"; return $ BinaryOp "/" (liftOp' (/)) (liftOp' (/)) }

negateExpression = do { char '-'; a <- powerExpression; space; return (Negate a) } 
  +++ powerExpression

powerExpression = chainl1 value op
  where op = do { symbol "^"; return $ BinaryOp "^" (liftOp' (**)) (liftOp' (**)) }

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

variableList = sepby1 variable (space >> symbol ",")

intList = sepby1 parseInt (space >> symbol ",")

printList = do { a <- expression; symbol ","; b <- printList; return ((CommaPrint a) : b) } +++ do { a <- expression; symbol ";"; b <- printList; return ((SemiPrint a) : b) } +++ do { a <- expression; space; return [(NewLineExpression a)] } +++ do { return [] }

function = do { caseInsensitiveSymbol "INT"; a <- parenExpression; return (Function "INT" a) } 
  +++ do { caseInsensitiveSymbol "RND"; a <- parenExpression; return (Function "RND" a) } +++ do { caseInsensitiveSymbol "TAB"; a <- parenExpression; return (Function "TAB" a)}

constant = parseInt +++ parseString

parseDim = do { caseInsensitiveSymbol "DIM"; a <- arrList; return (Dim a); }

parseEnd = do { caseInsensitiveSymbol "END"; space; return End }

parseFor = do { caseInsensitiveSymbol "FOR"; a <- parseId; symbol "="; b <- expression; caseInsensitiveSymbol "TO"; c <- expression; d <- parseStep;  return (For a b c d) }

parseStep = do { caseInsensitiveSymbol "STEP"; expression } +++ return (IntConst 1)

parseGoto = do { caseInsensitiveSymbol "GOTO"; a <- bInt; return (Goto a) }

parseGoSub = do { caseInsensitiveSymbol "GOSUB"; a <- bInt; return (GoSub a) }

parseIf = do { caseInsensitiveSymbol "IF"; a <- expression; caseInsensitiveSymbol "THEN"; b <- bInt; return (If a b) }

parseIfElse = do { caseInsensitiveSymbol "IF"; a <- expression; caseInsensitiveSymbol "THEN"; b <- bInt; caseInsensitiveSymbol "ELSE"; c <- bInt; return (IfElse a b c) }

parseInput = do { caseInsensitiveSymbol "INPUT"; a <- option "" bString; space; option ' ' (char ';'); space; b <- variableList; return (Input a b) }

parseLet = do { caseInsensitiveSymbol "LET"; a <- variable; symbol "="; space; b <- expression; return (Let a b) }

parseNext = do { caseInsensitiveSymbol "NEXT"; a <- idList; return (Next a) }

parseOn = do { caseInsensitiveSymbol "ON"; a <- expression; caseInsensitiveSymbol "GOTO"; b <- intList; return (On a b) }

parsePrint = do { caseInsensitiveSymbol "PRINT"; a <- printList; return (Print a) } +++ do { caseInsensitiveSymbol "PRINT"; return $ Print [] }

parseRem = do { caseInsensitiveSymbol "REM"; a <- many (sat isPrint); return (Rem a) }

parseReturn = do { caseInsensitiveSymbol "RETURN"; return Return }

parseAssignment = do { a <- variable; space; symbol "="; b <- expression; return (Assignment a b) }

statement = parseDim +++ parseEnd +++ parseFor +++ parseGoto +++ parseGoSub +++ parseIfElse +++ parseIf +++ parseInput +++ parseLet +++ parseNext +++ parseOn +++ parsePrint +++ parseRem +++ parseReturn +++ parseAssignment

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