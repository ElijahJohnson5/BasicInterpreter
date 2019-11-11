{-
 FIRST ATTEMPT AT INTERPRETING THE AST, DID NOT WORK HOW I WANTED IT TO STARTED OVER IN THE PARSER.HS FILE
-}

import Prelude hiding (lines)
import Parselib hiding (space, int)
import Data.Char
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Array
import Data.Array.IO
import System.IO
import System.Random
import Data.Maybe

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
  | PrintTab Expression [Expression]
  | Rem String
  | Return
  | Assignment Expression Expression deriving Show

data Line = Line Int [Statement] deriving Show

data Expression = BinaryOp String (Expression -> Expression -> Expression) Expression Expression
  | Not Expression
  | Negate Expression
  | Id Char | Function String Expression | IntConst Int | StringConst String | FloatConst Float | BoolConst Bool | Array Expression [Expression] | SemiPrint Expression | CommaPrint Expression | NewLineExpression Expression | Array1D { array1d :: (IOArray Int Expression) } | Array2D { array2d :: (IOArray (Int, Int) Expression) }

instance Show Expression where
  show (BinaryOp a _ b c) = show b ++ a ++ show c
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

liftRela op (IntConst a) (IntConst b) = BoolConst $ (fromIntegral a) `op` (fromIntegral b)
liftRela op (FloatConst a) (FloatConst b) = BoolConst $ a `op` b
liftRela op (FloatConst a) (IntConst b) = BoolConst $ a `op` (fromIntegral b)
liftRela op (IntConst a) (FloatConst b) = BoolConst $ (fromIntegral a) `op` b

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
  +++ do { symbol "RND"; a <- parenExpression; return (Function "RND" a) }

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

parsePrint = do { symbol "PRINT"; a <- printList; return (Print a) }

parsePrintTab = do { symbol "PRINT"; symbol "TAB"; a <- parenExpression; option ' ' (char ';'); b <- printList; return (PrintTab a b) }

parseRem = do { symbol "REM"; a <- many (sat isPrint); return (Rem a) }

parseReturn = do { symbol "RETURN"; return Return }

parseAssignment = do { a <- variable; space; symbol "="; b <- expression; return (Assignment a b) }

statement = parseDim +++ parseEnd +++ parseFor +++ parseGoto +++ parseGoSub +++ parseIf +++ parseInput +++ parseLet +++ parseNext +++ parseOn +++ parsePrintTab +++ parsePrint +++ parseRem +++ parseReturn +++ parseAssignment

statements = do { a <- statement; symbol ":"; b <- statements; return (a : b); } +++ do { a <- statement; return [a] } 

lines = do { a <- bInt; b <- statements; symbol "\n"; c <- lines; return ((Line a b) : c) } +++ do { a <- bInt; b <- statements; return [(Line a b)] }

lineMap lineList = IntMap.fromAscList (fmap (\l@(Line a _) -> (a, l)) lineList)

runProgram = do
  lMap <- ask
  runLine [] (snd (IntMap.findMin lMap))

  -- THIS IS SUPER UGLY
runLine exprList (Line a []) = do
  lMap <- ask
  runLine exprList (snd $ fromJust (IntMap.lookupGT a lMap))
runLine exprList (Line a (x:xs)) = do
  s <- runStatement x
  checkS s
    where checkS End = do 
            return [()]
          checkS (Goto n) = do
            lMap <- ask
            runLine exprList (lMap IntMap.! n)
          checkS gs@(GoSub b) = do
            lMap <- ask
            runLine ((gs,a) : exprList) (lMap IntMap.! b)
          checkS e@(If (BoolConst c) d) = do 
            lMap <- ask
            if (c) then runLine exprList (lMap IntMap.! d) else runLine exprList (Line a xs)
          checkS f@(For i b c d) = do
            liftIO $ putStrLn (show f)
            runStatement (Let i b)
            runLine ((f,a) : exprList) (Line a xs)
          checkS (Next i) = do
            evalFor (tail exprList) (head exprList) (Line a xs)
          checkS Return = do
            lMap <- ask
            runLine (tail exprList) (snd $ fromJust (IntMap.lookupGT (snd $ head exprList) lMap))
          checkS s = runLine exprList (Line a xs)

-- THIS IS ALSO SUPER UGLY
evalFor exprList e@((For a b c d), line) (Line l xs) = do 
  runStatement (Let a (BinaryOp "+" (liftOp (+)) a d))
  a' <- evalExpression a
  c' <- evalExpression c
  checkEnd a' c'
    where checkEnd (IntConst x) (IntConst y) = if (x <= y) then do
            lMap <- ask
            if (l == line) then runLine (e:exprList) (lMap IntMap.! l) else runLine (e:exprList) (snd $ fromJust (IntMap.lookupGT line lMap))
          else do 
            lMap <- ask
            runLine exprList (Line l xs)

create2DArray  :: (Int,Int) -> IO (IOArray (Int,Int) Expression)
create2DArray (x,y) = do
  b <- newArray ((1,1), (x + 1,y + 1)) (IntConst 0)
  return b

create1DArray  :: Int -> IO (IOArray Int Expression)
create1DArray x = do
  b <- newArray (1,(x + 1)) (IntConst 0)
  return b

insertArray :: Expression -> ReaderT (IntMap.IntMap Line) (StateT (Map.Map Char Expression) IO) ()
insertArray (Array (Id a) ((IntConst x):(IntConst y):[])) = do
  b <- lift $ lift $ create2DArray (x,y)
  modify (Map.insert a (Array2D b))
  return ()
insertArray (Array (Id a) [(IntConst x)]) = do
  b <- lift $ lift $ create1DArray x
  modify (Map.insert a (Array1D b))
  return ()

updateArray :: Expression -> Expression -> ReaderT (IntMap.IntMap Line) (StateT (Map.Map Char Expression) IO) ()
updateArray (Array (Id a) ((IntConst x):(IntConst y):[])) val = do
  symbolMap <- get
  lift $ lift $ (writeArray (array2d (symbolMap Map.! a)) (x,y) val)
  return ()
updateArray (Array (Id a) [(IntConst x)]) val = do
  symbolMap <- get
  lift $ lift $ (writeArray (array1d (symbolMap Map.! a)) x val)
  return ()

evalArray (Array a xs) = do
  b <- sequence (fmap evalExpression xs)
  insertArray (Array a b)

insertSymbol :: Expression -> Expression -> ReaderT (IntMap.IntMap Line) (StateT (Map.Map Char Expression) IO) ()
insertSymbol (Id a) val = do 
  modify (Map.insert a val)
  return ()

runStatement :: Statement -> ReaderT (IntMap.IntMap Line) (StateT (Map.Map Char Expression) IO) Statement
runStatement s@(Let (Array id expr) b) = do
  b' <- evalExpression b
  expr' <- sequence (fmap evalExpression expr)
  updateArray (Array id expr') b'
  return s
runStatement s@(Let c@(Id a) b) = do
  b' <- evalExpression b
  insertSymbol c b'
  return s
runStatement s@(Dim arrList) = do
  sequence (fmap evalArray arrList)
  return s
runStatement s@(If a b) = do
  a' <- evalExpression a
  return (If a' b)
runStatement s@(Print []) = do
  liftIO $ putStr "\n"
  return s
runStatement s@(Print xs) = do 
  loop xs
  return s
    where loop [] = return ()
          loop ((NewLineExpression x):xs) = do
            x' <- evalExpression x
            liftIO $ putStrLn (show x')
          loop ((CommaPrint x):xs) = do
            x' <- evalExpression x
            liftIO $ putStr (show x' ++ " ")
            loop xs
          loop ((SemiPrint x):xs) = do 
            x' <- evalExpression x
            liftIO $ putStr (show x' ++ " ")
            loop xs 

runStatement s@(On a xs) = do 
  a' <- evalExpression a
  n <- gotoList a' xs
  derefrence n
    where calcGoto n xs = head ((foldr (.) id (replicate (n) tail)) xs)
          gotoList (IntConst n) xs = do
            x <- evalExpression (calcGoto n xs)
            return x
          derefrence (IntConst x) = do
            return (Goto x)
runStatement s@(Input str expr) = do
  liftIO $ putStr (str ++ " ")
  a <- liftIO $ getLine
  let b = fmap (read :: String -> Int) (sepLineBy a ',')
  insertMultipleSymbols expr (fmap IntConst b)
  return s
runStatement s@(Assignment c@(Id a) b) = do
  b' <- evalExpression b
  insertSymbol c b'
  return s
runStatement s@(Assignment (Array id expr) b) = do
  b' <- evalExpression b
  expr' <- sequence (fmap evalExpression expr)
  updateArray (Array id expr') b'
  return s
runStatement s = do
  return s

insertMultipleSymbols [] [] = return ()
insertMultipleSymbols (i:ids) (v:vals) = do
  insertSymbol i v
  insertMultipleSymbols ids vals

sepLineBy line sep = case dropWhile (== sep) line of
                      "" -> []
                      s' -> w : sepLineBy s'' sep
                            where (w, s'') = break (== sep) s'

evalExpression :: Expression -> ReaderT (IntMap.IntMap Line) (StateT (Map.Map Char Expression) IO) Expression
evalExpression t@(BinaryOp _ op a b) = do
  a' <- evalExpression a
  b' <- evalExpression b
  return (op a' b')
evalExpression (Not a) = do
  a' <- evalExpression a
  return ((liftBoolUnary not) a')
evalExpression (Negate a) = do 
  a' <- evalExpression a
  return ((liftUnary negate) a')
evalExpression (Id a) = do 
  symbolMap <- get
  return (symbolMap Map.! a)
evalExpression (Array (Id a) (b:c:[])) = do
  symbolMap <- get
  b' <- evalExpression b
  c' <- evalExpression c
  lift $ lift $ readFrom2D (symbolMap Map.! a) (b', c')
evalExpression (Array (Id a) [b]) = do
  symbolMap <- get
  b' <- evalExpression b
  lift $ lift $ readFrom1D (symbolMap Map.! a) b'
evalExpression (Function name expr) = do
  a <- evalExpression expr
  b <- lift $ lift $ evalFunByName name $ a
  return b
    where evalFunByName name = case name of
                            "INT" -> basicInt
                            "RND" -> basicRnd
evalExpression a = return a

basicInt :: Expression -> IO Expression
basicInt (FloatConst a) = do 
  return (IntConst . floor $ a)
basicInt a = return a

basicRnd :: Expression -> IO Expression
basicRnd (IntConst a) = do 
  a <- (randomRIO (0,1) :: IO Float)
  return (FloatConst a)

readFrom2D :: Expression -> (Expression, Expression) -> IO (Expression)
readFrom2D (Array2D a) ((IntConst x), (IntConst y)) = do 
  c <- readArray a (x, y)
  return c

readFrom1D :: Expression -> Expression -> IO (Expression)
readFrom1D (Array1D a) (IntConst x) = do
  c <- readArray a x
  return c

basic s = do 
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  fs <- readFile s
  runStateT (runReaderT runProgram (lineMap ((fst $ head $ parse lines fs)))) (Map.empty :: Map.Map Char Expression)
  return (parse lines fs)
