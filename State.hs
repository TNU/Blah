module State (
    Runtime,
    Scope,
    getVar,
    setVar,
    addToHeap,
    getFromHeap,
    setHeap,
    getHeap,
    getSysFunc,

    isEOF,
    readLine,
    showStr,
    showLine,
    usingState,
    usingIO,
    run,
    newRuntime,
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Error

import qualified System.IO as IO
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Failure
import Value
import Func
import Heap

type Scope          = Map.Map String Value
type StateData      = (Scope, SystemFuncMap, Heap)
type RuntimeState   = StateT StateData IO
type Runtime        = Failable RuntimeState

{- State Modifiers -}
getVar ::  String -> Runtime Value
getVar name = usingState get >>= find
   where find (scope, _, _) = toVal (Map.lookup name scope)
         toVal (Just val)   = return val
         toVal Nothing      = evalFail  $ "variable " ++ (show name)
                                       ++ " does not exist"

setVar :: String -> Value -> Runtime ()
setVar name val = usingState . modify $ set
    where set (scope, funcs, heap) = (Map.insert name val scope, funcs, heap)

getSysFunc :: String -> Runtime SystemFunc
getSysFunc name = usingState get >>= find
    where find (_, funcs, _) = toVal (Map.lookup name funcs)
          toVal (Just func)  = return func
          toVal Nothing      = evalFail $ "system function " ++ (show name)
                                       ++ " does not exist"

getFromHeap :: Int -> Runtime Value
getFromHeap index = do heap <- getHeap
                       if hasIndex heap index
                       then return (extract heap index)
                       else evalFail $ "heap index out of bounds at "
                                    ++ (show index)

addToHeap :: Value -> Runtime Int
addToHeap val = do heap <- getHeap
                   let (newHeap, index) = insert heap val
                   setHeap newHeap
                   return index

getHeap :: Runtime Heap
getHeap = usingState get >>= onlyHeap
    where onlyHeap (_, _, heap) = return heap

setHeap :: Heap -> Runtime ()
setHeap = usingState . modify . set
    where set heap (scope, funcs, _) = (scope, funcs, heap)


{- IO Operations -}
readLine :: Runtime String
readLine = usingIO getLine

isEOF :: Runtime Bool
isEOF = usingIO IO.isEOF

showStr :: String -> Runtime ()
showStr = usingIO . putStrLn

showLine :: String -> Runtime ()
showLine = usingIO . putStrLn

usingState :: RuntimeState a -> Runtime a
usingState = lift

usingIO :: IO a -> Runtime a
usingIO = lift . lift

run :: Runtime a -> StateData -> IO (Either Failure a)
run = evalStateT . runErrorT

newRuntime :: Scope -> SystemFuncMap -> StateData
newRuntime scope funcs = (scope, funcs, newHeap)
