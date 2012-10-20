module Runner (
    RuntimeIO,
    Value,
    newRuntime,
    getInput,
    updateInput,
    evalStmt,
    showStr,
) where

import Control.Monad (liftM, liftM2, join)
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Data.Functor.Identity

import qualified Data.Map as Map

import Failure
import Parser

data Value = Vi Integer
           | Vb Bool
           deriving (Show)

type Scope           = Map.Map String Value
type RuntimeState    = State (Scope, String)
type RuntimeIO a     = RuntimeState (IO a)
type RuntimeFailable = FailableM RuntimeState


newRuntime :: String -> (Scope, String)
newRuntime input = (Map.empty, input)

{- Show Functions -}
showValOrErr :: RuntimeIO a -> Either Failure Value -> RuntimeIO a
showValOrErr doRest (Right val)  = showVal doRest val
showValOrErr doRest (Left error) = showStr doRest error

showOnlyErr :: RuntimeIO a -> Either Failure () -> RuntimeIO a
showOnlyErr doRest (Right ())   = doRest
showOnlyErr doRest (Left error) = showStr doRest error

showVal :: RuntimeIO a -> Value -> RuntimeIO a
showVal doRest (Vi int)  = showRaw doRest int
showVal doRest (Vb bool) = showRaw doRest bool 

showRaw :: (Show t) => RuntimeIO a -> t -> RuntimeIO a
showRaw doRest x = showStr doRest (show x)

showStr :: RuntimeIO a -> String -> RuntimeIO a
showStr doRest msg = doRest >>= return . (putStrLn msg >>)

{- Eval -}
evalStmt ::  RuntimeIO a -> Stmt -> RuntimeIO a
evalStmt doRest (Assn name orop) = extractVal (evalOrop orop) >>= assign doRest name
evalStmt doRest (So orop) = extractVal (evalOrop orop) >>= showValOrErr doRest

extractVal :: RuntimeFailable (Bool, Value) -> RuntimeState (Either Failure Value)
extractVal orop = runErrorT orop >>= return . liftM snd

assign :: RuntimeIO a -> String -> Either Failure Value -> RuntimeIO a
assign doRest name (Right val)  = runErrorT (setVar name val) >>= showOnlyErr doRest
assign doRest name (Left error) = showStr doRest error

{- Decl Evaluators -}
evalOrop :: OrOp -> RuntimeFailable (Bool, Value)
evalOrop (Oa x)    = evalAndOp x
evalOrop (Or x y)  = applyBinOp doOr (evalOrop x) (evalAndOp y)
    where doOr a@(t,_) b = if t then return a else return b

evalAndOp :: AndOp -> RuntimeFailable (Bool, Value)
evalAndOp (Ac x)    = evalComp x >>= toAndOp
evalAndOp (And x y) = applyBinOp doAnd (evalAndOp x) (evalComp y)
    where doAnd a@(t,_) b = if t then toAndOp b else return a
    
toAndOp :: (Bool, Value, Value) -> RuntimeFailable (Bool, Value)
toAndOp (b, x, _)   = return (b, x)
          
evalComp :: Comp -> RuntimeFailable (Bool, Value, Value)
evalComp (Ca x)     = evalArth x >>= toComp
evalComp (Lt x y)   = applyBinOp (compOp (<))  (evalComp x) (evalArth y)
evalComp (Eq x y)   = applyBinOp (compOp (==)) (evalComp x) (evalArth y)
evalComp (Gt x y)   = applyBinOp (compOp (>))  (evalComp x) (evalArth y)
evalComp (Lte x y)  = applyBinOp (compOp (<=)) (evalComp x) (evalArth y)
evalComp (Gte x y)  = applyBinOp (compOp (>=)) (evalComp x) (evalArth y)
evalComp (Ne x y)   = applyBinOp (compOp (/=)) (evalComp x) (evalArth y) 

toComp :: Value -> RuntimeFailable (Bool, Value, Value)
toComp x@(Vi 0) = return (False, x, x)
toComp x@(Vi _) = return (True,  x, x)
toComp x@(Vb y) = return (y,     x, x)

compOp :: (Integer -> Integer -> Bool) -> (Bool, Value, Value) -> Value 
        -> RuntimeFailable (Bool, Value, Value)
compOp op (old, _, Vi x) (Vi y) = return (new, Vb new, Vi y)
    where result = x `op` y
          new = old && result

evalArth :: Arth -> RuntimeFailable Value
evalArth (At x)     = evalTerm x
evalArth (Add x y)  = applyBinOp add (evalArth x) (evalTerm y)
    where add (Vi x) (Vi y) = return . Vi $ (x + y)
evalArth (Sub x y)  = applyBinOp sub (evalArth x) (evalTerm y)
    where sub (Vi x) (Vi y) = return . Vi $ (x - y)

evalTerm :: Term -> RuntimeFailable Value
evalTerm (Tf x) = evalFact x
evalTerm (Mult x y) = applyBinOp mult (evalTerm x) (evalFact y)
    where mult (Vi x) (Vi y) = return . Vi $ (x * y)
evalTerm (Div x y) = applyBinOp divide (evalTerm x) (evalFact y)
    where divide (Vi x) (Vi 0) = evalFail $ "divide by zero"
          divide (Vi x) (Vi y) = return . Vi $ (x `div` y)

evalFact :: Factor -> RuntimeFailable Value
evalFact (Fb x) = return . Vb $ x
evalFact (Fp x) = evalNeg x
evalFact (Fn x) = (evalNeg x) >>= negateVal
   where negateVal (Vi x) = return . Vi . negate $ x

evalNeg :: Neg -> RuntimeFailable Value
evalNeg (Ni x) = return . Vi $ x
evalNeg (Nd x) = getVar x
evalNeg (Np x) = evalParen x

evalParen :: Paren -> RuntimeFailable Value
evalParen (Po x) = evalOrop x >>= return . snd

{- State Modifiers -}
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

{- Utility Functions -}
applyBinOp :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
applyBinOp binOp a b = join (liftM2 binOp a b)