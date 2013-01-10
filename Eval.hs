module Eval (
    Expr,
    runExpr,
    testExpr,
    elemAssn,
) where

import Control.Monad (liftM, liftM2, liftM3, join)
import Control.Monad.Trans.State
import Control.Monad.Trans.Error

import qualified Data.Foldable as Fold
import qualified Data.Sequence as Seq

import Failure (evalFail, typeFail1, typeFail2)
import Parser
import State
import Converters
import Properties

type Expr = OrOp

elemAssn :: Elem -> Value -> Runtime ()
elemAssn (ElemS str i)   v = setElemHelper (return (Vs str)) (runExpr i) v
elemAssn (ElemD name i)  v = setElemHelper (getVar name) (runExpr i) v
elemAssn (ElemL list i)  v = setElemHelper (evalList list) (runExpr i) v
elemAssn (ElemP paren i) v = setElemHelper (evalParen paren) (runExpr i) v
elemAssn (ElemC call i)  v = setElemHelper (evalCall call) (runExpr i) v
elemAssn (ElemO obj i)   v = setElemHelper (evalObj obj) (runExpr i) v
elemAssn (ElemE elem i)  v = setElemHelper (evalElem elem) (runExpr i) v

setElemHelper :: Runtime Value -> Runtime Value -> Value -> Runtime ()
setElemHelper a b c = join (liftM3 setElem a b (return c))

setElem :: Value -> Value -> Value -> Runtime ()
setElem (Vrl r) (Vi i) v = do (Vl list) <- getFromHeap r
                              if i >= 0 && i < Seq.length list
                              then setAtHeap r . Vl $ (Seq.update i v list)
                              else evalFail "index out of bounds"
setElem (Vrl index) _ value = evalFail "list indexing only supports integers"
setElem (Vs str)  (Vi i) _  = evalFail "cannot assign to string"
setElem (Vs str)  _      _  = evalFail "list indexing only supports integers"
setElem x           _ value = typeFail1 "indexing" x

{- orop -}
evalOrop :: OrOp -> Runtime (Bool, Value)
evalOrop (Oa x)    = evalAndOp x
evalOrop (Or x y)  = applyBinOp doOr (evalOrop x) (evalAndOp y)
    where doOr a@(t,_) b = if t then return a else return b

{- andop -}
evalAndOp :: AndOp -> Runtime (Bool, Value)
evalAndOp (An x)    = evalNot x
evalAndOp (And x y) = applyBinOp doAnd (evalAndOp x) (evalNot y)
    where doAnd a@(t,_) b = if t then return b else return a

{- notop -}
evalNot :: NotOp -> Runtime (Bool, Value)
evalNot (Mc x)      = evalComp x >>= toNotOp
evalNot (Not x)     = evalComp x >>= toNotOp >>= doNot
    where doNot (bool, value) = makeNotOp (not bool)
          makeNotOp bool = return (bool, Vb bool)

toNotOp :: (Bool, Value, Value) -> Runtime (Bool, Value)
toNotOp (_, _, x) = toBool x >>= \bool -> return (bool, x)

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

eqOp Vnothing Vnothing = return True
eqOp (Vi x) (Vi y)             = return (x == y)
eqOp (Vb x) (Vb y)             = return (x == y)
eqOp (Vs x) (Vs y)             = return (x == y)
eqOp (Vsf _ x) (Vsf _ y)       = return (x == y)
eqOp (Vbsf _ i x) (Vbsf _ j y) = return $ (i == j) && (x == y)
eqOp x      y                  = return False

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
evalFact (Fl x)     = evalList x
evalFact (Fp x)     = evalNeg x
evalFact (Fn x)     = evalNeg x >>= negateVal
    where negateVal (Vi x)   = return . Vi . negate $ x
          negateVal x        = typeFail1 "negation" x

{- list -}
evalList :: List -> Runtime Value
evalList list = Vrl `liftM` (makeList list >>= addToHeap)
    where makeList Lempty         = return (Vl Seq.empty)
          makeList (Lone x)       = (Vl . Seq.singleton) `liftM` runExpr x
          makeList (Lcons list x) = liftM2 append (runExpr x) (makeList list)
          append val (Vl list) = Vl $ (list Seq.|> val)

{- neg -}
evalNeg :: Neg -> Runtime Value
evalNeg (Ni x) = return . Vi $ x
evalNeg (Nd x) = getVar x
evalNeg (Nc x) = evalCall x
evalNeg (No x) = evalObj x
evalNeg (Np x) = evalParen x
evalNeg (Ne x) = evalElem x

{- call -}
evalCall :: Call -> Runtime Value
evalCall (Call neg args) = applyBinOp callFunc (evalNeg neg) (evalArgs args)
    where callFunc (Vsf  func _) vals   = func vals
          callFunc (Vbsf func i _) vals = func i vals
          callFunc x         _          = typeFail1 "function call" x

{- args -}
evalArgs :: Args -> Runtime [Value]
evalArgs Rempty = return []
evalArgs (Rcons args expr) = liftM2 (:) (runExpr expr) (evalArgs args)

{- object -}
evalObj :: Obj -> Runtime Value
evalObj (PropS string prop) = return (Vs string) >>= valProp prop
evalObj (PropD name   prop) = getVar name        >>= valProp prop
evalObj (PropL list   prop) = evalList list      >>= valProp prop
evalObj (PropP paren  prop) = evalParen paren    >>= valProp prop
evalObj (PropO obj    prop) = evalObj obj        >>= valProp prop
evalObj (PropE elem   prop) = evalElem elem      >>= valProp prop

{- elem -}
evalElem :: Elem -> Runtime Value
evalElem (ElemS str expr) = applyBinOp getElem (return (Vs str)) (runExpr expr)
evalElem (ElemD name expr) = applyBinOp getElem (getVar name) (runExpr expr)
evalElem (ElemL list expr) = applyBinOp getElem (evalList list) (runExpr expr)
evalElem (ElemP pare expr) = applyBinOp getElem (evalParen pare) (runExpr expr)
evalElem (ElemC call expr) = applyBinOp getElem (evalCall call) (runExpr expr)
evalElem (ElemO obj  expr) = applyBinOp getElem (evalObj obj)   (runExpr expr)
evalElem (ElemE elem expr) = applyBinOp getElem (evalElem elem) (runExpr expr)

getElem :: Value -> Value -> Runtime Value
getElem (Vs string) (Vi i) = if i >= 0 && i < length string
                             then return . Vs $ [string !! i]
                             else evalFail "index out of bounds"
getElem (Vs str) _         = evalFail "string indexing only supports integers"
getElem (Vrl index) (Vi i) = do (Vl list) <- getFromHeap index
                                if i >= 0 && i < Seq.length list
                                then return (Seq.index list i)
                                else evalFail "index out of bounds"
getElem (Vrl index) _      = evalFail  "list indexing only supports integers"
getElem x         _        = typeFail1 "indexing" x


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

runExpr :: Expr -> Runtime Value
runExpr expr = snd `liftM` evalOrop expr

testExpr :: Expr -> Runtime Bool
testExpr expr = fst `liftM` evalOrop expr
