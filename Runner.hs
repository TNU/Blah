module Runner (
    Runtime,
    Value,
    updateInput,
    getInput,
    printVal,
    printStr,
    newRuntime,
    evalExpr,
    evalStmt,
) where

import Control.Monad (liftM, liftM2)
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map

import Parser

data Value = Vi Integer
           deriving (Show)
           
type Scope   = Map.Map String Value
type Runtime = (Scope, String)
               
printVal :: Value -> State Runtime [IO ()] -> State Runtime [IO ()]
printVal (Vi int) doRest = printRaw int doRest

printRaw :: (Show t) => t -> State Runtime [IO ()] -> State Runtime [IO ()]
printRaw x doRest = printStr (show x) doRest

printStr :: String -> State Runtime [IO ()] -> State Runtime [IO ()]
printStr msg doRest = doRest >>= return . ((putStrLn msg):)

newRuntime :: String -> Runtime
newRuntime input = (Map.empty, input)

evalStmt :: (Error e, MonadError e m) => Stmt -> State Runtime (m ())
evalStmt (Assn name expr) = evalExpr expr >>= assign name
    where assign name (Right val)   = setVar name val
          assign name (Left error)  = return . throwError $ error

evalExpr :: (Error e, MonadError e m) => Expr -> State Runtime (m Value)
evalExpr (Et x)     = evalTerm x
evalExpr (Add x y)  = applyBinOp add (evalExpr x) (evalTerm y)
    where add (Vi x) (Vi y) = return . Vi $ (x + y)
evalExpr (Sub x y)  = applyBinOp sub (evalExpr x) (evalTerm y)
    where sub (Vi x) (Vi y) = return . Vi $ (x - y)

evalTerm :: (Error e, MonadError e m) => Term -> State Runtime (m Value)
evalTerm (Tf x) = evalFact x
evalTerm (Mult x y) = applyBinOp mult (evalTerm x) (evalFact y)
    where mult (Vi x) (Vi y) = return . Vi $ (x * y)
evalTerm (Div x y) = applyBinOp divide (evalTerm x) (evalFact y)
    where divide (Vi x) (Vi 0) = evalFail $ "divide by zero"
          divide (Vi x) (Vi y) = return . Vi $ (x `div` y)
    
evalFact :: (Error e, MonadError e m) => Factor -> State Runtime (m Value)
evalFact (Fp x) = evalNeg x
evalFact (Fn x) = applyOp negateVal (evalNeg x)
   where negateVal (Vi x) = return . Vi . negate $ x

evalNeg :: (Error e, MonadError e m) => Neg -> State Runtime (m Value)
evalNeg (Ni x) = returnVal . Vi $ x
evalNeg (Nd x) = getVar x
evalNeg (Np x) = evalParen x

evalParen :: (Error e, MonadError e m) => Paren -> State Runtime (m Value)
evalParen (Pe x) = evalExpr x

returnVal :: (Error e, MonadError e m) => Value -> State Runtime (m Value)
returnVal = return . return

evalFail :: (Error e, MonadError e m) => String -> m a
evalFail = throwError . strMsg . ("<eval> " ++)


getInput :: State Runtime String
getInput = state $ \(s, i) -> (i, (s, i))

updateInput :: String -> State Runtime ()
updateInput newInput = state $ \(s, i) -> ((), (s, newInput))

getVar :: (Error e, MonadError e m) =>  String -> State Runtime (m Value)
getVar name = state $ \(scope, i) -> (toVal (Map.lookup name scope), (scope, i))
        where toVal (Just val) = return val
              toVal Nothing    = evalFail $ (show name) ++ " does not exist"

setVar :: (Error e, MonadError e m) => String -> Value -> State Runtime (m ())
setVar name val = state $ \(scope, i) -> (return (), (Map.insert name val scope, i))


-- apply unary operation to a state return value
applyOp :: (Error e, MonadError e m, Monad s) => 
                (a -> m a) -> s (m a) -> s (m a)
applyOp op x = liftM (>>= op) x 

-- apply binary operation to two state return values
applyBinOp :: (Error e, MonadError e m, Monad s) => 
                (a -> a -> m a) -> s (m a) -> s (m a) -> s (m a)
applyBinOp op a b = liftM2 (\c d -> c >>= (d >>=) . op) a b
