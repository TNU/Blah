module Func (
    SystemFunc(..),
    SystemFuncMap(..),
    basicFuncVars,
    basicFuncs,
    runSystemFunc,
) where

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Error

import qualified Data.Foldable as Fold
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Failure
import Value
import Heap

type SystemFunc     = Func
type SystemFuncMap  = FuncMap

type FailableIO = Failable IO
type Func      = Heap -> [Value] -> FailableIO (Value, Heap)
type FuncMap   = Map.Map String SystemFunc

type BoundFunc = Value -> Func

basicFuncVars :: Map.Map String Value
basicFuncVars = toFuncVars basicFuncs

basicFuncs :: FuncMap
basicFuncs = Map.fromList [
        ("writeln", writeln),
        ("write", write),
        ("readchr", readchr),
        ("readln", readln)
    ]

{- io functions -}
write :: Func
write heap values = lift (printMany values) >> returnNothing heap
    where printMany values = foldr printOne (return ()) values
          printOne value io = io >> putStr (toStr heap value)

writeln :: Func
writeln heap values = lift (printMany values) >> returnNothing heap
    where printMany values = foldr printOne (return ()) values
          printOne value io = io >> putStrLn (toStr heap value)

readchr :: Func
readchr heap _ = toReturn `liftM` lift getChar
    where toReturn chr = (Vs (chr:""), heap)

readln :: Func
readln heap _ = toReturn `liftM` lift getLine
    where toReturn str = (Vs str, heap)

funcFail :: String -> FailableIO Value
funcFail = throwError . strMsg

runSystemFunc :: (MonadIO m) => Func -> Heap -> [Value]
                                     -> Failable m (Value, Heap)
runSystemFunc func heap args = liftIO (runErrorT result) >>= handleErrors
    where result = func heap args
          handleErrors (Right result) = return result
          handleErrors (Left error)  = evalFail error

{- Utility Functions -}
toFuncVars :: FuncMap -> Map.Map String Value
toFuncVars = Map.mapWithKey mapper
    where mapper name _ = Vf name

returnNothing :: Heap -> FailableIO (Value, Heap)
returnNothing heap = return (Vnothing, heap)

{- Func Converters -}
toStr :: Heap -> Value -> String
toStr heap Vnothing    = "Nothing"
toStr heap (Vi int)    = show int
toStr heap (Vb bool)   = show bool
toStr heap (Vs string) = string
toStr heap (Vl list)   = listToRepr heap list
toStr heap (Vf name)   = name ++ "(..)"
toStr heap (Vrl index) = toStr heap (extract heap index)

toRepr :: Heap -> Value -> String
toRepr heap Vnothing  = "Nothing"
toRepr heap (Vi int)  = show int
toRepr heap (Vb bool) = show bool
toRepr heap (Vl list) = listToRepr heap list
toRepr heap (Vf name) = name ++ "(..)"
toRepr heap (Vrl index) = toRepr heap (extract heap index)
toRepr heap (Vs string) = "'" ++ concatMap esc string ++ "'"
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

listToRepr :: Heap -> Seq.Seq Value -> String
listToRepr heap seq = "[" ++ toReprElems ++ "]"
    where toReprElems = Fold.concat (Seq.mapWithIndex showOne seq)
          showOne 0 x = toRepr heap x
          showOne _ x = ", " ++ toRepr heap x
