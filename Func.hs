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

import qualified Data.Map as Map

import Failure
import Value

type SystemFunc     = Func
type SystemFuncMap  = FuncMap

type FailableIO = Failable IO
type Func      = [Value] -> FailableIO Value
type FuncMap   = Map.Map String SystemFunc

basicFuncVars :: Map.Map String Value
basicFuncVars = toFuncVars basicFuncs

basicFuncs :: FuncMap
basicFuncs = Map.fromList [
        ("writeln", writeln),
        ("write", write),
        ("readchr", readchr),
        ("readln", readln)
    ]

write :: Func
write values = lift printValues >> discard
    where printValues = mapM_ (putStr . toStr) values

writeln :: Func
writeln values = lift printValues >> discard
    where printValues = mapM_ (putStrLn . toStr) values

readchr :: Func
readchr _ = toStrVal `liftM` lift getChar
    where toStrVal = Vs . (:"")

readln :: Func
readln _ = toStrVal `liftM` lift getLine
    where toStrVal = Vs

funcFail :: String -> FailableIO Value
funcFail = throwError . strMsg

runSystemFunc :: (MonadIO m) => Func -> [Value] -> Failable m Value
runSystemFunc func args = liftIO (runErrorT (func args)) >>= convertErrors
    where convertErrors (Right value) = return value
          convertErrors (Left error)  = evalFail error

toFuncVars :: FuncMap -> Map.Map String Value
toFuncVars = Map.mapWithKey mapper
    where mapper name _ = Vf name

discard :: FailableIO Value
discard = return Vnothing

