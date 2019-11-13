import Parser
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Data.Array
import Data.Array.IO
import System.IO
import System.Random
import Data.Maybe

lineMap lineList = IntMap.fromList lineList

type Interpreter a = ReaderT (IntMap.IntMap [Statement]) (StateT (Map.Map String Expression) IO) a

runProgram :: Interpreter [Statement]
runProgram = do
  lMap <- ask
  evalStatements [] (IntMap.findMin lMap)

evalStatements :: [(Int, Statement)] -> (Int, [Statement]) -> Interpreter [Statement]
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

forCheck :: Expression -> Int -> Expression -> Expression -> Expression -> Interpreter Statement
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

evalStatement :: Statement -> Interpreter Statement
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


evalExpression :: Expression -> Interpreter Expression
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
printStatement ((NewLineExpression a):xs) = do
  liftIO $ putStrLn (show a)
  printStatement xs
printStatement ((CommaPrint a):xs) = do
  liftIO $ putStr ((show a) ++ "\t")
  printStatement xs
printStatement ((SemiPrint a):xs) = do
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

insertArray :: Expression -> Interpreter ()
insertArray (Array (Id a) ((IntConst x):(IntConst y):[])) = do
  b <- lift $ lift $ create2DArray (x,y)
  modify (Map.insert (a : "arr") (Array2D b))
  return ()
insertArray (Array (Id a) [(IntConst x)]) = do
  b <- lift $ lift $ create1DArray x
  modify (Map.insert (a : "arr") (Array1D b))
  return ()

updateArray :: Expression -> Expression -> Interpreter ()
updateArray (Array (Id a) ((IntConst x):(IntConst y):[])) val = do
  symbolMap <- get
  lift $ lift $ (writeArray (array2d (symbolMap Map.! (a : "arr"))) (x,y) val)
  return ()
updateArray (Array (Id a) [(IntConst x)]) val = do
  symbolMap <- get
  lift $ lift $ (writeArray (array1d (symbolMap Map.! (a : "arr"))) x val)
  return ()

insertSymbol :: Expression -> Expression -> Interpreter ()
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
  program <- parseBasicFile s
  runStateT (runReaderT runProgram (lineMap program)) (Map.empty :: Map.Map String Expression)
  return ()

basicTest s = do 
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  program <- parseTest s
  runStateT (runReaderT runProgram (lineMap program)) (Map.empty :: Map.Map String Expression)
  return ()