module Runner (
    Runtime,
    Value,
    getInput,
    putInput,
    evalExpr,
    printVal,
    printStr
) where

import Control.Monad (liftM2)
import Control.Monad.Error
import Control.Monad.State

import Parser
           
type Runtime = String

data Value = Vi Integer
           deriving (Show)
               
printVal :: Value -> State Runtime [IO ()] -> State Runtime [IO ()]
printVal (Vi int) doRest = printRaw int doRest

printRaw :: (Show t) => t -> State Runtime [IO ()] -> State Runtime [IO ()]
printRaw x doRest = printStr (show x) doRest

printStr :: String -> State Runtime [IO ()] -> State Runtime [IO ()]
printStr msg doRest = doRest >>= return . ((putStrLn msg):)

getInput :: State Runtime String
getInput = state $ \(input) -> (input, input)

putInput :: String -> State Runtime ()
putInput newInput = state $ \(input) -> ((), newInput)

evalExpr :: (Error e, MonadError e m) => Expr -> State Runtime (m Value)
evalExpr (Et x) = evalTerm x
evalExpr (Add x y) = applyBinOp add (evalExpr x) (evalTerm y)
    where add (Vi x) (Vi y) = return . Vi $ (x + y)
evalExpr (Sub x y) = applyBinOp sub (evalExpr x) (evalTerm y)
    where sub (Vi x) (Vi y) = return . Vi $ (x - y)

evalTerm :: (Error e, MonadError e m) => Term -> State Runtime (m Value)
evalTerm (Tf x) = evalFact x
evalTerm (Mult x y) = applyBinOp mult (evalTerm x) (evalFact y)
    where mult (Vi x) (Vi y) = return . Vi $ (x * y)
evalTerm (Div x y) = applyBinOp divide (evalTerm x) (evalFact y)
    where divide (Vi x) (Vi 0) = evalFail $ "divide by zero"
          divide (Vi x) (Vi y) = return . Vi $ (x `div` y)
    
applyBinOp :: (Error e, MonadError e m, Monad s) => 
                (a -> a -> m a) -> s (m a) -> s (m a) -> s (m a)
applyBinOp op a b = liftM2 (apply op) a b
        where apply binOp x y = do 
                a <- x
                b <- y
                binOp a b
    
evalFact :: (Error e, MonadError e m) => Factor -> State Runtime (m Value)
evalFact (Fi x) = returnVal . Vi $ x
evalFact (Fn x) = evalNeg x
evalFact (Fp x) = evalParen x

evalNeg :: (Error e, MonadError e m) => Neg -> State Runtime (m Value)
evalNeg (Ni x) = returnVal . Vi . negate $ x
evalNeg (Np x) = evalParen x

evalParen :: (Error e, MonadError e m) => Paren -> State Runtime (m Value)
evalParen (Pe x) = evalExpr x

returnVal :: (Error e, MonadError e m) => Value -> State Runtime (m Value)
returnVal = return . return

evalFail :: (Error e, MonadError e m) => String -> m a
evalFail = throwError . strMsg . ("<eval> " ++)