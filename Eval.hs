module Eval (
    Expr,
    runExpr,
    testExpr,
) where

import Control.Monad (liftM, liftM2, join)
import Control.Monad.Trans.State
import Control.Monad.Trans.Error

import qualified Data.Foldable as Fold
import qualified Data.Sequence as Seq

import Value
import Func
import Failure
import State
import Parser
import Converters

type Expr = OrOp

{- orop -}
evalOrop :: OrOp -> Runtime (Bool, Value)
evalOrop (Oa x)    = evalAndOp x
evalOrop (Or x y)  = applyBinOp doOr (evalOrop x) (evalAndOp y)
    where doOr a@(t,_) b = if t then return a else return b

{- andop -}
evalAndOp :: AndOp -> Runtime (Bool, Value)
evalAndOp (Ac x)    = evalComp x >>= toAndOp
evalAndOp (And x y) = applyBinOp doAnd (evalAndOp x) (evalComp y)
    where doAnd a@(t,_) b = if t then toAndOp b else return a

toAndOp :: (Bool, Value, Value) -> Runtime (Bool, Value)
toAndOp (_, _, x) = toBool x >>= \bool -> return (bool, x)

{- comp -}
evalComp :: Comp -> Runtime (Bool, Value, Value)
evalComp (Cj x)     = evalJoin x >>= toComp
evalComp (Lt x y)   = doComp ltOp (evalComp x) (evalJoin y)
evalComp (Eq x y)   = doComp eqOp (evalComp x) (evalJoin y)
evalComp (Gt x y)   = doComp gtOp (evalComp x) (evalJoin y)
    where gtOp      = flip ltOp
evalComp (Lte x y)  = doComp lteOp(evalComp x) (evalJoin y)
    where lteOp x y = not `liftM` ltOp y x
evalComp (Gte x y)  = doComp gteOp (evalComp x) (evalJoin y)
    where gteOp x y = not `liftM` ltOp x y
evalComp (Neq x y)  = doComp neOp (evalComp x) (evalJoin y)
    where neOp x y  = not `liftM` eqOp x y

toComp :: Value -> Runtime (Bool, Value, Value)
toComp x = deref x >>= \y -> return (True, y, x)

doComp :: (Value -> Value -> Runtime Bool) -> Runtime (Bool, Value, Value)
                                           -> Runtime Value
                                           -> Runtime (Bool, Value, Value)
doComp op a b = do (old, x, _) <- a
                   y           <- b >>= deref
                   result      <- x `op` y
                   let new = result && old
                   return (new, y, Vb new)

ltOp :: Value -> Value -> Runtime Bool
ltOp (Vi x)   (Vi y)   = return (x < y)
ltOp (Vs x)   (Vs y)   = return (x < y)
ltOp a@(Vl x) b@(Vl y) = allTrue [allLte, notLonger, notEqual]
    where mFalse    = return False
          mTrue     = return True
          eachGt    = Seq.zipWith ltOnVals y x
          ltOnVals c d = applyBinOp ltOp (deref c) (deref d)
          allLte    = not `liftM` (Fold.foldr (liftM2 (||)) mFalse eachGt)
          notLonger = return (Seq.length x <= Seq.length y)
          notEqual  = not `liftM` eqOp a b
          allTrue   = foldr (liftM2 (&&)) mTrue
ltOp x      y       = typeFail2 "comparison" x y

eqOp :: Value -> Value -> Runtime Bool
eqOp (Vl x) (Vl y) = (lengthEq &&) `liftM` allEq
    where lengthEq = Seq.length x == Seq.length y
          allEq    = Fold.foldr (liftM2 (&&)) (return True) eachEq
          eachEq   = Seq.zipWith eqOnVals x y
          eqOnVals a b = applyBinOp eqOp (deref a) (deref b)
eqOp x      y      = return (x == y)

{- join -}
evalJoin :: Join -> Runtime Value
evalJoin (Ja x)       = evalArth x
evalJoin (Concat x y) = applyBinOpOnVal conc (evalJoin x) (evalArth y)
    where conc x y = Vs `liftM` liftM2 (++) (toStr x) (toStr y)

{- arth -}
evalArth :: Arth -> Runtime Value
evalArth (At x)     = evalTerm x
evalArth (Add x y)  = applyBinOpOnVal add (evalArth x) (evalTerm y)
    where add (Vi x) (Vi y) = return . Vi $ (x + y)
          add x      y      = typeFail2 "addition" x y
evalArth (Sub x y)  = applyBinOpOnVal sub (evalArth x) (evalTerm y)
    where sub (Vi x) (Vi y) = return . Vi $ (x - y)
          sub x      y      = typeFail2 "subtraction" x y

{- term -}
evalTerm :: Term -> Runtime Value
evalTerm (Tf x) = evalFact x
evalTerm (Mult x y) = applyBinOpOnVal mult (evalTerm x) (evalFact y)
    where mult (Vi x) (Vi y) = return . Vi $ (x * y)
          mult x      y      = typeFail2 "multiplication" x y
evalTerm (Div x y) = applyBinOpOnVal divide (evalTerm x) (evalFact y)
    where divide (Vi x) (Vi 0) = evalFail "divide by zero"
          divide (Vi x) (Vi y) = return . Vi $ (x `div` y)
          divide x      y      = typeFail2 "division" x y

{- fact -}
evalFact :: Factor -> Runtime Value
evalFact (Fnothing) = return Vnothing
evalFact (Fb x)     = return . Vb $ x
evalFact (Fs x)     = return . Vs $ x
evalFact (Fl x)     = Vrl `liftM` (evalList x >>= addToHeap)
evalFact (Fp x)     = evalNeg x
evalFact (Fn x)     = evalNeg x >>= negateVal
    where negateVal (Vi x)   = return . Vi . negate $ x
          negateVal x        = typeFail1 "negation" x

evalList :: List -> Runtime Value
evalList Lempty         = return (Vl Seq.empty)
evalList (Lone x)       = (Vl . Seq.singleton) `liftM` runExpr x
evalList (Lcons list x) = liftM2 append (runExpr x) (evalList list)
    where append val (Vl list) = Vl $ (list Seq.|> val)

{- neg -}
evalNeg :: Neg -> Runtime Value
evalNeg (Ni x) = return . Vi $ x
evalNeg (Nd x) = getVar x
evalNeg (Nc x) = evalCall x
evalNeg (No x) = evalObj x
evalNeg (Np x) = evalParen x

{- call -}
evalCall :: Call -> Runtime Value
evalCall (Call neg args) = applyBinOp callFunc (evalNeg neg) (evalArgs args)
    where callFunc (Vf name) vals = getSysFunc name >>= runFunc vals
          callFunc x         _    = typeFail1 "function call" x

runFunc :: [Value] -> SystemFunc -> Runtime Value
runFunc args func = do heap <- getHeap
                       (value, newHeap) <- runSystemFunc func heap args
                       setHeap newHeap
                       return value

{- args -}
evalArgs :: Args -> Runtime [Value]
evalArgs Rempty = return []
evalArgs (Rcons args expr) = liftM2 (:) (runExpr expr) (evalArgs args)

{- object -}
evalObj :: Obj -> Runtime Value
evalObj (PropS string prop) = strProp prop string
evalObj (PropD name   prop) = getVar name >>= valProp prop
evalObj (PropL list   prop) = evalList list >>= valProp prop
evalObj (PropP paren  prop) = evalParen paren >>= valProp prop
evalObj (PropO obj    prop) = evalObj obj >>= valProp prop
evalObj (PropE elem   prop) = return . Vs $ "not implemented"

valProp :: String -> Value -> Runtime Value
valProp prop (Vs str)   = strProp prop str
valProp prop (Vl list)  = listProp prop list
valProp prop (Vrl ref)  = listRefProp prop ref
valProp prop x          = typeFail1 "property lookup" x

strProp :: String -> String -> Runtime Value
strProp "length" = return . Vi . fromIntegral . length

listProp :: String -> Seq.Seq Value -> Runtime Value
listProp "length" = return . Vi . fromIntegral . Seq.length

listRefProp :: String -> Int -> Runtime Value
listRefProp prop ref = getFromHeap ref >>= doAccessors
    where doAccessors (Vl seq) = listProp prop seq

{- paren -}
evalParen :: Paren -> Runtime Value
evalParen (Pe x) = runExpr x

{- Utility Functions -}
applyBinOpOnVal :: (Value -> Value -> Runtime a) ->
                  Runtime Value -> Runtime Value -> Runtime a
applyBinOpOnVal binOp a b = do da <- a >>= deref
                               db <- b >>= deref
                               binOp da db

applyBinOp :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
applyBinOp binOp a b = join (liftM2 binOp a b)

typeFail1 :: String -> Value -> Runtime a
typeFail1 opName x   = evalFail $ opName ++ " of \"" ++ (show x) ++ "\" "
                                ++ "is not supported"

typeFail2 :: String -> Value -> Value -> Runtime a
typeFail2 opName x y = evalFail $ opName ++ " of \""
                              ++ (show x) ++ "\" and \"" ++ (show y) ++ "\" "
                              ++ "is not supported"

runExpr :: Expr -> Runtime Value
runExpr expr = snd `liftM` evalOrop expr

testExpr :: Expr -> Runtime Bool
testExpr expr = fst `liftM` evalOrop expr
