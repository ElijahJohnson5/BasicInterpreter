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
  show (IntConst a) = "IntConst: " ++ show a
  show (StringConst a) = a
  show (FloatConst a) = "FloatConst: " ++ show a
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

lineMap lineList = IntMap.fromList lineList

runProgram :: ReaderT (IntMap.IntMap [Statement]) (StateT (Map.Map String Expression) IO) [Statement]
runProgram = do
  lMap <- ask
  evalStatements [] (IntMap.findMin lMap)

evalStatements :: [(Int, Statement)] -> (Int, [Statement]) -> ReaderT (IntMap.IntMap [Statement]) (StateT (Map.Map String Expression) IO) [Statement]
evalStatements stack (line,[]) = do
  lMap <- ask
  evalStatements stack (fromJust (IntMap.lookupGT line lMap))
evalStatements (f@(line1,(For (Id a) ini1 to1 step1)):stack) (line,(s@(For (Id b) ini2 to2 step2):ss)) = 
  if (a == b && line1 == line) then do 
    evalStatements (f : stack) (line, ss)
  else do 
    s' <- evalStatement s
    case s' of
      NoOp -> forSkip stack (line, ss)
      otherwise -> evalStatements ((line,s') : f : stack) (line, ss)
evalStatements stack (line,(s@(For (Id b) ini2 to2 step2):ss)) = do
    s' <- evalStatement s
    case s' of 
      NoOp -> forSkip stack (line, ss)
      otherwise -> evalStatements ((line,s') : stack) (line, ss)
evalStatements (f@(line,(For id ini to step)):stack) (b,(s@(Next (i:is)):ss)) = do
  id' <- evalExpression id
  a <- forCheck id line id' to step
  case a of 
    Goto n -> do 
      lMap <- ask
      evalStatements (f:stack) (n, (lMap IntMap.! n))
    NoOp -> evalStatements stack (b,(Next is):ss)
evalStatements stack (b,gs@(GoSub n):ss) = do
  lMap <- ask
  evalStatements ((b,gs):stack) (n, (lMap IntMap.! n))
evalStatements (gs@(line,(GoSub n)):stack) (b,(s@(Return):ss)) = do
  lMap <- ask
  evalStatements stack (fromJust (IntMap.lookupGT line lMap))
evalStatements stack (line,(s@(If a b):ss)) = do 
  s' <- evalStatement s
  case s' of 
    Goto n -> do 
      lMap <- ask
      evalStatements stack (n,(lMap IntMap.! n))
    NoOp -> evalStatements stack (line,ss)
evalStatements stack (line,(s:ss)) = do
  s' <- evalStatement s
  case s' of 
    End -> return []
    Goto n -> do 
      lMap <- ask
      evalStatements stack (n, (lMap IntMap.! n))
    otherwise -> evalStatements stack (line,ss)


ifStatement (BoolConst a) b = if a then (Goto b) else NoOp

forCheck :: Expression -> Int -> Expression -> Expression -> Expression -> ReaderT (IntMap.IntMap [Statement]) (StateT (Map.Map String Expression) IO) Statement
forCheck id line (IntConst a) to step = do
  step' <- evalExpression step
  insertSymbol id (IntConst (a + (intConst step')))
  to' <- evalExpression to
  if (boolConst to') then return (Goto line) else return (NoOp)

forSkip stack (l,[]) = do 
  lMap <- ask 
  forSkip stack (fromJust (IntMap.lookupGT l lMap))
forSkip stack (l,((Next i):ss)) = evalStatements stack (l, ss)
forSkip stack (l,(s:ss)) = forSkip stack (l,ss) 

evalStatement :: Statement -> ReaderT (IntMap.IntMap [Statement]) (StateT (Map.Map String Expression) IO) Statement
evalStatement (If a b) = do
  a' <- evalExpression a
  return (ifStatement a' b)
evalStatement s@(For id ini to step) = do
  ini' <- evalExpression ini
  step' <- evalExpression step
  to' <- evalExpression to
  insertSymbol id ini'
  res <- forHelper id ini' to to' step step'
  return res
evalStatement a@(Goto n) = return a 
evalStatement s@(On x gotos) = do
  x' <- evalExpression x
  onGotoHelper x' gotos
evalStatement s@(Print []) = do
  liftIO $ putStrLn ""
  return s
evalStatement s@(Print xs) = do
  xs' <- mapM evalExpression xs
  printStatement xs'
  return s
evalStatement s@(Let id@(Id a) b) = do
  b' <- evalExpression b
  insertSymbol id b'
  return s
evalStatement s@(Assignment id@(Id a) b) = do
  b' <- evalExpression b
  insertSymbol id b'
  return s
evalStatement s@(Let arr@(Array a b) c) = do
  b' <- mapM evalExpression b
  c' <- evalExpression c
  updateArray (Array a b') c'
  return s
evalStatement s@(Assignment arr@(Array a b) c) = do
  b' <- mapM evalExpression b
  c' <- evalExpression c
  updateArray (Array a b') c'
  return s
evalStatement s@(Dim xs) = do
  dimHelper xs
  return s
evalStatement s@(Input str expr) = do 
  liftIO $ putStr (str ++ " ")
  a <- liftIO $ getLine
  let b = fmap (read :: String -> Int) (sepLineBy a ',')
  insertMultipleSymbols expr (fmap IntConst b)
  return s
evalStatement s = return s

forHelper id ini' to to' step step' = do
  if (intConst step') > 0 then 
      if (intConst ini') <= (intConst to') then
          return (For id ini' (BinaryOp "<=" (liftRela (<=)) id to) step)
        else 
          return NoOp
    else 
      if (intConst ini') >= (intConst to') then
          return (For id ini' (BinaryOp ">=" (liftRela (>=)) id to) step)
        else
          return NoOp

onGotoHelper _ [] = error "Invalid on goto"
onGotoHelper (IntConst 1) ((IntConst x):xs) = return (Goto x)
onGotoHelper (IntConst y) ((IntConst x):xs) = onGotoHelper (IntConst (y - 1)) xs

sepLineBy line sep = case dropWhile (== sep) line of
  "" -> []
  s' -> w : sepLineBy s'' sep
        where (w, s'') = break (== sep) s'

insertMultipleSymbols [] [] = return ()
insertMultipleSymbols (i:ids) (v:vals) = do
  insertSymbol i v
  insertMultipleSymbols ids vals

dimHelper [] = return []
dimHelper ((Array a b):xs) = do
  b' <- mapM evalExpression b
  insertArray (Array a b')
  dimHelper xs


evalExpression :: Expression -> ReaderT (IntMap.IntMap [Statement]) (StateT (Map.Map String Expression) IO) Expression
evalExpression (BinaryOp str op a b) = do
  a' <- evalExpression a
  b' <- evalExpression b
  return (op a' b')
evalExpression (Not a) = do
  a' <- evalExpression a
  return (liftBoolUnary (not) a')
evalExpression (Negate a) = do
  a' <- evalExpression a
  return (liftUnary (negate) a')
evalExpression (Id a) = do
  symbolMap <- get
  return (symbolMap Map.! [a])
evalExpression arr@(Array (Id a) b) = do
  b' <- mapM evalExpression b
  symbolMap <- get
  lift $ lift $ readFromArray (symbolMap Map.! (a : "arr")) b'
evalExpression (Function name a) = do
  a' <- evalExpression a
  b <- lift $ lift $ getBasicFunction name $ a'
  return b
evalExpression (NewLineExpression a) = printExpressionHelper NewLineExpression a
evalExpression (CommaPrint a) = printExpressionHelper CommaPrint a
evalExpression (SemiPrint a) = printExpressionHelper SemiPrint a
evalExpression a = return a

printExpressionHelper constructor a = do
  a' <- evalExpression a 
  return $ constructor a'

printStatement [] = return ()
printStatement ((NewLineExpression (StringConst a):xs)) = do
  liftIO $ putStrLn a
  printStatement xs
printStatement ((NewLineExpression (IntConst a):xs)) = do
  liftIO $ putStrLn (show a)
  printStatement xs
printStatement ((NewLineExpression (FloatConst a):xs)) = do
  liftIO $ putStrLn (show a)
  printStatement xs
printStatement ((CommaPrint (StringConst a)):xs) = do
  liftIO $ putStr (a ++ "\t")
  printStatement xs
printStatement ((CommaPrint (IntConst a)):xs) = do
  liftIO $ putStr (show a ++ "\t")
  printStatement xs
printStatement ((CommaPrint (FloatConst a)):xs) = do
  liftIO $ putStr (show a ++ "\t")
  printStatement xs
printStatement ((SemiPrint (StringConst a)):xs) = do
  liftIO $ putStr a
  printStatement xs
printStatement ((SemiPrint (IntConst a)):xs) = do
  liftIO $ putStr (show a)
  printStatement xs
printStatement ((SemiPrint (FloatConst a)):xs) = do
  liftIO $ putStr (show a)
  printStatement xs


getBasicFunction name = case name of
  "RND" -> basicRnd
  "INT" -> basicInt
  "TAB" -> basicTab

basicInt :: Expression -> IO Expression
basicInt (FloatConst a) = do 
  return (IntConst . floor $ a)
basicInt a = return a

basicRnd :: Expression -> IO Expression
basicRnd (IntConst a) = do 
  a' <- (randomRIO (0, (fromIntegral a)) :: IO Float)
  return (FloatConst a')

basicTab :: Expression -> IO Expression
basicTab (IntConst a) = return (StringConst $ replicate a ' ')

create2DArray  :: (Int,Int) -> IO (IOArray (Int,Int) Expression)
create2DArray (x,y) = do
  b <- newArray ((0,0), (x,y)) (IntConst 0)
  return b

create1DArray  :: Int -> IO (IOArray Int Expression)
create1DArray x = do
  b <- newArray (0,x) (IntConst 0)
  return b

insertArray :: Expression -> ReaderT (IntMap.IntMap [Statement]) (StateT (Map.Map String Expression) IO) ()
insertArray (Array (Id a) ((IntConst x):(IntConst y):[])) = do
  b <- lift $ lift $ create2DArray (x,y)
  modify (Map.insert (a : "arr") (Array2D b))
  return ()
insertArray (Array (Id a) [(IntConst x)]) = do
  b <- lift $ lift $ create1DArray x
  modify (Map.insert (a : "arr") (Array1D b))
  return ()

updateArray :: Expression -> Expression -> ReaderT (IntMap.IntMap [Statement]) (StateT (Map.Map String Expression) IO) ()
updateArray (Array (Id a) ((IntConst x):(IntConst y):[])) val = do
  symbolMap <- get
  lift $ lift $ (writeArray (array2d (symbolMap Map.! (a : "arr"))) (x,y) val)
  return ()
updateArray (Array (Id a) [(IntConst x)]) val = do
  symbolMap <- get
  lift $ lift $ (writeArray (array1d (symbolMap Map.! (a : "arr"))) x val)
  return ()

insertSymbol :: Expression -> Expression -> ReaderT (IntMap.IntMap [Statement]) (StateT (Map.Map String Expression) IO) ()
insertSymbol (Id a) val = do 
  modify (Map.insert [a] val)
  return ()

readFromArray :: Expression -> [Expression] -> IO (Expression)
readFromArray (Array2D a) ((IntConst x):(IntConst y):[]) = do 
  c <- readArray a (x, y)
  return c
readFromArray (Array1D a) [(IntConst x)] = do
  c <- readArray a x
  return c

basic s = do 
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  fs <- readFile s
  runStateT (runReaderT runProgram (lineMap ((fst $ head $ parse lines fs)))) (Map.empty :: Map.Map String Expression)
  return ()

parseTest s = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  fs <- readFile s
  putStrLn fs
  return (fst $ head $ parse lines fs)