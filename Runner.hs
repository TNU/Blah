module Runner (
    RuntimeIO,
    Value,
    newRuntime,
    runExpr,
    setVar,
    getVar,
    getInput,
    updateInput,
    showValOrErr,
    showOnlyErr,
    showVal,
    showRaw,
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
           deriving (Show, Eq)

type Scope           = Map.Map String Value
type RuntimeState    = State (Scope, String)
type RuntimeIO a     = RuntimeState (IO a)
type RuntimeFailable = FailableM RuntimeState

type Expr = OrOp

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

{- Expr -}
runExpr :: Expr -> RuntimeState (Either Failure Value)
runExpr expr = runErrorT (evalOrop expr) >>= return . liftM snd

{- Orop -}
evalOrop :: OrOp -> RuntimeFailable (Bool, Value)
evalOrop (Oa x)    = evalAndOp x
evalOrop (Or x y)  = applyBinOp doOr (evalOrop x) (evalAndOp y)
    where doOr a@(t,_) b = if t then return a else return b

{- AndOp -}
evalAndOp :: AndOp -> RuntimeFailable (Bool, Value)
evalAndOp (Ac x)    = evalComp x >>= toAndOp
evalAndOp (And x y) = applyBinOp doAnd (evalAndOp x) (evalComp y)
    where doAnd a@(t,_) b = if t then toAndOp b else return a

toAndOp :: (Bool, Value, Value) -> RuntimeFailable (Bool, Value)
toAndOp (_, _, x@(Vi 0)) = return (False, x)
toAndOp (_, _, x@(Vi _)) = return (True, x)
toAndOp (_, _, x@(Vb b)) = return (b, x)

{- Comp -}
evalComp :: Comp -> RuntimeFailable (Bool, Value, Value)
evalComp (Ca x)     = evalArth x >>= toComp
evalComp (Lt x y)   = doComp ltOp (evalComp x) (evalArth y)
evalComp (Eq x y)   = doComp eqOp (evalComp x) (evalArth y)
evalComp (Gt x y)   = doComp gtOp (evalComp x) (evalArth y)
    where gtOp      = flip ltOp
evalComp (Lte x y)  = doComp lteOp(evalComp x) (evalArth y)
    where lteOp x y = ltOp y x >>= return . not
evalComp (Gte x y)  = doComp gteOp (evalComp x) (evalArth y)
    where gteOp x y = ltOp x y >>= return . not
evalComp (Ne x y)   = doComp neOp (evalComp x) (evalArth y)
    where neOp x y  = eqOp x y >>= return . not

toComp :: Value -> RuntimeFailable (Bool, Value, Value)
toComp x = return (True, x, x)

doComp :: (Value -> Value -> RuntimeFailable Bool)
                                    -> RuntimeFailable (Bool, Value, Value)
                                    -> RuntimeFailable Value
                                    -> RuntimeFailable (Bool, Value, Value)
doComp op a b = do (old, x, _)  <- a
                   y            <- b
                   result       <- x `op` y
                   let new = result && old
                   return (new, y, Vb new)

ltOp :: Value -> Value -> RuntimeFailable Bool
ltOp (Vi x) (Vi y)  = return (x < y)
ltOp x      y       = typeFail2 "comparison" x y

eqOp :: Value -> Value -> RuntimeFailable Bool
eqOp x y = return (x == y)

{- Arth -}
evalArth :: Arth -> RuntimeFailable Value
evalArth (At x)     = evalTerm x
evalArth (Add x y)  = applyBinOp add (evalArth x) (evalTerm y)
    where add (Vi x) (Vi y) = return . Vi $ (x + y)
          add x      y      = typeFail2 "addition" x y
evalArth (Sub x y)  = applyBinOp sub (evalArth x) (evalTerm y)
    where sub (Vi x) (Vi y) = return . Vi $ (x - y)
          sub x      y      = typeFail2 "subtraction" x y

{- Term -}
evalTerm :: Term -> RuntimeFailable Value
evalTerm (Tf x) = evalFact x
evalTerm (Mult x y) = applyBinOp mult (evalTerm x) (evalFact y)
    where mult (Vi x) (Vi y) = return . Vi $ (x * y)
          mult x      y      = typeFail2 "multiplication" x y
evalTerm (Div x y) = applyBinOp divide (evalTerm x) (evalFact y)
    where divide (Vi x) (Vi 0) = evalFail "divide by zero"
          divide (Vi x) (Vi y) = return . Vi $ (x `div` y)
          divide x      y      = typeFail2 "division" x y

{- Fact -}
evalFact :: Factor -> RuntimeFailable Value
evalFact (Fb x) = return . Vb $ x
evalFact (Fp x) = evalNeg x
evalFact (Fn x) = (evalNeg x) >>= negateVal
   where negateVal (Vi x) = return . Vi . negate $ x
         negateVal x      = typeFail1 "negation" x

{- Neg -}
evalNeg :: Neg -> RuntimeFailable Value
evalNeg (Ni x) = return . Vi $ x
evalNeg (Nd x) = getVar x
evalNeg (Np x) = evalParen x

{- Paren -}
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

typeFail1 :: String -> Value -> RuntimeFailable a
typeFail1 opName x   = evalFail $ opName ++ " of \"" ++ (show x) ++ "\" "
                                ++ "is not supported"

typeFail2 :: String -> Value -> Value -> RuntimeFailable a
typeFail2 opName x y = evalFail $ opName ++ " of \""
                              ++ (show x) ++ "\" and \"" ++ (show y) ++ "\" "
                              ++ "is not supported"

