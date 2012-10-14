module Runner (
    RuntimeIO,
    Value,
    newRuntime,
    getInput,
    updateInput,
    evalStmt,
    showStr,
) where

import Control.Monad (liftM2, join)
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Data.Functor.Identity

import qualified Data.Map as Map

import Failure
import Parser

data Value = Vi Integer
           deriving (Show)

type Scope           = Map.Map String Value
type RuntimeState    = State (Scope, String)
type RuntimeIO a     = RuntimeState (IO a)
type RuntimeFailable = FailableM RuntimeState


newRuntime :: String -> (Scope, String)
newRuntime input = (Map.empty, input)

showValOrErr :: RuntimeIO a -> Either Failure Value -> RuntimeIO a
showValOrErr doRest (Right val)  = showVal doRest val
showValOrErr doRest (Left error) = showStr doRest error

showOnlyErr :: RuntimeIO a -> Either Failure () -> RuntimeIO a
showOnlyErr doRest (Right ())   = doRest
showOnlyErr doRest (Left error) = showStr doRest error

showVal :: RuntimeIO a -> Value -> RuntimeIO a
showVal doRest (Vi int) = showRaw doRest int

showRaw :: (Show t) => RuntimeIO a -> t -> RuntimeIO a
showRaw doRest x = showStr doRest (show x)

showStr :: RuntimeIO a -> String -> RuntimeIO a
showStr doRest msg = doRest >>= return . (putStrLn msg >>)

evalStmt ::  RuntimeIO a -> Stmt -> RuntimeIO a
evalStmt doRest (Assn name expr) = runErrorT (evalExpr expr) >>= assign doRest name
evalStmt doRest (Se expr) = runErrorT (evalExpr expr) >>= showValOrErr doRest

assign :: RuntimeIO a -> String -> Either Failure Value -> RuntimeIO a
assign doRest name (Right val)  = runErrorT (setVar name val) >>= showOnlyErr doRest
assign doRest name (Left error) = showStr doRest error


evalExpr :: Expr -> RuntimeFailable Value
evalExpr (Et x)     = evalTerm x
evalExpr (Add x y)  = applyBinOp add (evalExpr x) (evalTerm y)
    where add (Vi x) (Vi y) = return . Vi $ (x + y)
evalExpr (Sub x y)  = applyBinOp sub (evalExpr x) (evalTerm y)
    where sub (Vi x) (Vi y) = return . Vi $ (x - y)

evalTerm :: Term -> RuntimeFailable Value
evalTerm (Tf x) = evalFact x
evalTerm (Mult x y) = applyBinOp mult (evalTerm x) (evalFact y)
    where mult (Vi x) (Vi y) = return . Vi $ (x * y)
evalTerm (Div x y) = applyBinOp divide (evalTerm x) (evalFact y)
    where divide (Vi x) (Vi 0) = throwError . evalError $ "divide by zero"
          divide (Vi x) (Vi y) = return . Vi $ (x `div` y)

evalFact :: Factor -> RuntimeFailable Value
evalFact (Fp x) = evalNeg x
evalFact (Fn x) = (evalNeg x) >>= negateVal
   where negateVal (Vi x) = return . Vi . negate $ x

evalNeg :: Neg -> RuntimeFailable Value
evalNeg (Ni x) = return . Vi $ x
evalNeg (Nd x) = getVar x
evalNeg (Np x) = evalParen x

evalParen :: Paren -> RuntimeFailable Value
evalParen (Pe x) = evalExpr x


getInput :: RuntimeState String
getInput = state $ \(s, i) -> (i, (s, i))

updateInput :: String -> RuntimeState ()
updateInput newInput = state $ \(s, i) -> ((), (s, newInput))

getVar ::  String -> RuntimeFailable Value
getVar name = ErrorT . state $ action
        where action (scope, i) = (toVal (Map.lookup name scope), (scope, i))
              toVal (Just val)  = Right val
              toVal Nothing     = Left . evalError $ "variable " ++ (show name) ++ " does not exist"

setVar :: String -> Value -> RuntimeFailable ()
setVar name val = ErrorT . state $ action
        where action (scope, i) = (return (), (Map.insert name val scope, i))


applyBinOp :: (Monad m) => (a -> a -> m a) -> m a -> m a -> m a
applyBinOp binOp a b = join (liftM2 binOp a b)
