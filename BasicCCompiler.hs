import Parser
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Map

data C = Headers { headers :: [String] } | MainBody { mainBody :: [String] } | Functions { functions :: [String] }

type CCompiler a = State (C,[String]) a

{-
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
-}

compileStatement :: [Statement] -> CCompiler [Statement]
compileStatement [] = return []
compileStatement ((Let (Id a) b):ss) = do
  state <- get
  if elem [a] (snd state) then do
      str <- compileExpression b
      put (((fst state) ++ [a] ++ " = " ++ str ++ ";\n"), snd state)
      compileStatement ss
    else do
      str <- compileExpression b
      put (((fst state) ++ "int " ++ [a] ++ " = " ++ str ++ ";\n"), [a] : (snd state))
      compileStatement ss 
compileStatement (s:ss) = compileStatement ss      

{-
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
-}

compileExpression :: Expression -> CCompiler String
compileExpression (BinaryOp str op exprA exprB) = do
  a <- compileExpression exprA
  b <- compileExpression exprB
  case str of
    "<>" -> return (a ++ " != " ++ b)
    "=" -> return (a ++ " == " ++ b)
    "^" -> return ("pow(" ++ a ++ "," ++ b ++ ")")
    otherwise -> return (a ++ " " ++ str ++ " " ++ b)
compileExpression a = return (show a)

{-
compile s = do
  lines <- parseTest s
  runState (compileStatement (snd $ head $ lines)) ("", (empty :: Map String Expression))
-}