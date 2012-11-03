module Eval (
    RuntimeState,
    newRuntime,

    Value(..),
    Expr,
    evalExpr,

    setVar,
    getVar,
    getInput,
    updateInput,

    toBool,
) where

import Control.Monad (liftM, liftM2, join)
import Control.Monad.Trans.State
import Control.Monad.Trans.Error

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Failure
import Parser

data Value = Vi Integer
           | Vb Bool
           | Vs String
           | Vl (Seq.Seq Value)
           | Vc Integer
           | Vnothing
           deriving (Ord, Eq)

instance Show Value where
    show = str

type Scope           = Map.Map String Value
type RuntimeState    = State (Scope, String)
type RuntimeFailable = FailableM RuntimeState

type Expr = OrOp

evalExpr :: Expr -> RuntimeFailable (Bool, Value)
evalExpr = evalOrop


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
toAndOp (_, _, x) = toBool x >>= \b -> return (b, x)

{- Comp -}
evalComp :: Comp -> RuntimeFailable (Bool, Value, Value)
evalComp (Cj x)     = evalJoin x >>= toComp
evalComp (Lt x y)   = doComp ltOp (evalComp x) (evalJoin y)
evalComp (Eq x y)   = doComp eqOp (evalComp x) (evalJoin y)
evalComp (Gt x y)   = doComp gtOp (evalComp x) (evalJoin y)
    where gtOp      = flip ltOp
evalComp (Lte x y)  = doComp lteOp(evalComp x) (evalJoin y)
    where lteOp x y = not `liftM` ltOp y x
evalComp (Gte x y)  = doComp gteOp (evalComp x) (evalJoin y)
    where gteOp x y = not `liftM` ltOp x y
evalComp (Neq x y)   = doComp neOp (evalComp x) (evalJoin y)
    where neOp x y  = not `liftM` eqOp x y

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
ltOp (Vi x) (Vi y)     = return (x < y)
ltOp (Vs x) (Vs y)     = return (x < y)
ltOp a@(Vl x) b@(Vl y) = allTrue [allLte, notLonger, notEqual]
    where false     = return False
          true      = return True
          eachGt    = Seq.zipWith ltOp y x
          allLte    = not `liftM` (Fold.foldr (liftM2 (||)) false eachGt)
          notLonger = return (Seq.length x <= Seq.length y)
          notEqual  = not `liftM` eqOp a b
          allTrue   = foldr (liftM2 (&&)) true
ltOp x      y       = typeFail2 "comparison" x y

eqOp :: Value -> Value -> RuntimeFailable Bool
eqOp x y = return (x == y)

{- Join -}
evalJoin :: Join -> RuntimeFailable Value
evalJoin (Ja x)       = evalArth x
evalJoin (Concat x y) = applyBinOp conc (evalJoin x) (evalArth y)
    where conc x y = return . Vs $ ((show x) ++ (show y))

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
evalFact (Fnothing) = return Vnothing
evalFact (Fb x)     = return . Vb $ x
evalFact (Fs x)     = return . Vs $ x
evalFact (Fl x)     = evalList x
evalFact (Fp x)     = evalNeg x
evalFact (Fn x)     = (evalNeg x) >>= negateVal
    where negateVal (Vi x) = return . Vi . negate $ x
          negateVal x      = typeFail1 "negation" x

evalList :: List -> RuntimeFailable Value
evalList Lempty         = return . Vl $ Seq.empty
evalList (Lone x)       = (Vl . Seq.singleton . snd) `liftM` evalExpr x
evalList (Lcons list x) = applyBinOp append (evalExpr x) (evalList list)
    where append (b, val) (Vl list) = return . Vl $ (list Seq.|> val)

{- Neg -}
evalNeg :: Neg -> RuntimeFailable Value
evalNeg (Ni x) = return . Vi $ x
evalNeg (Nd x) = getVar x
evalNeg (Nc x) = evalCall x
evalNeg (Np x) = evalParen x

evalCall :: Call -> RuntimeFailable Value
evalCall (Call neg args) = evalNeg neg

{- Args -}
evalArgs :: Args -> RuntimeFailable [Value]
evalArgs Rempty = return []
evalArgs (Rcons args expr) = liftM2 (:) (snd `liftM` evalExpr expr) (evalArgs args)

{- Paren -}
evalParen :: Paren -> RuntimeFailable Value
evalParen (Pe x) = snd `liftM` evalExpr x

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

{- Converters -}
toBool :: Value -> RuntimeFailable Bool
toBool (Vi 0)   = return False
toBool (Vi x)   = return True
toBool (Vb x)   = return x
toBool (Vs "")  = return False
toBool (Vs x)   = return True
toBool (Vl x)   = return . not . Seq.null $ x
toBool Vnothing = return False

str :: Value -> String
str Vnothing  = "Nothing"
str (Vi int)  = show int
str (Vb bool) = show bool
str (Vs string) = string
str (Vl list) = reprList list

repr :: Value -> String
repr Vnothing  = "Nothing"
repr (Vi int)  = show int
repr (Vb bool) = show bool
repr (Vl list) = reprList list
repr (Vs string) = "'" ++ concatMap esc string ++ "'"
    where esc '\a' = "\\a"
          esc '\b' = "\\b"
          esc '\f' = "\\f"
          esc '\n' = "\\n"
          esc '\r' = "\\r"
          esc '\t' = "\\t"
          esc '\v' = "\\v"
          -- " is not escaped
          esc '\'' = "\\\'"
          esc '\\' = "\\\\"
          esc '\0' = "\\0"
          esc x    = [x]

reprList :: Seq.Seq Value -> String
reprList seq = "[" ++ reprElems ++ "]"
    where reprElems = Fold.concat (Seq.mapWithIndex showOne seq)
          showOne 0 x = repr x
          showOne _ x = ", " ++ repr x

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

{- New Runtime -}
newRuntime :: String -> (Scope, String)
newRuntime input = (defaultVars, input)

defaultVars :: Map.Map String Value
defaultVars = Map.fromList [
    ]


