module State (
    Value(..),
    Runtime,

    getVar,
    setVar,
    addToHeap,
    setAtHeap,
    getFromHeap,
    setHeap,
    getHeap,

    isEOF,
    readLine,
    showStr,
    showLine,
    usingState,
    usingIO,
    run,
    newRuntime,
) where

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Error

import qualified System.IO as IO
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

import Failure
import Memory

type Scope          = Map.Map String Value
type Heap           = Memory Value
type StateData      = (Scope, Heap)
type RuntimeState   = StateT StateData IO
type Runtime        = Failable RuntimeState

type SysFunc        = [Value] -> Runtime Value
type BoundSysFunc   = Int -> [Value] -> Runtime Value

{- Value -}
data Value = Vnothing
           | Vi Int
           | Vb Bool
           | Vs String
           | Vl (Seq.Seq Value)
           | Vrl Int
           | Vsf SysFunc String
           | Vbsf BoundSysFunc Int String

{- State Modifiers -}
setVar :: String -> Value -> Runtime ()
setVar name val = usingState . modify $ set
    where set (scope, heap) = (Map.insert name val scope, heap)

getVar ::  String -> Runtime Value
getVar name = usingState get >>= find
    where find (scope, _) = toVal (Map.lookup name scope)
          toVal (Just val)   = return val
          toVal Nothing      = evalFail  $ "variable " ++ (show name)
                                        ++ " does not exist"

addToHeap :: Value -> Runtime Int
addToHeap val = do heap <- getHeap
                   let (newHeap, index) = insert heap val
                   setHeap newHeap
                   return index

setAtHeap :: Int -> Value -> Runtime ()
setAtHeap index val = do heap <- getHeap
                         if hasIndex heap index
                         then setHeap (update heap index val)
                         else evalFail $ "heap index out of bounds at "
                                      ++ (show index)

getFromHeap :: Int -> Runtime Value
getFromHeap index = do heap <- getHeap
                       if hasIndex heap index
                       then return (extract heap index)
                       else evalFail $ "heap index out of bounds at "
                                    ++ (show index)

getHeap :: Runtime Heap
getHeap = usingState get >>= onlyHeap
    where onlyHeap (_, heap) = return heap

setHeap :: Heap -> Runtime ()
setHeap = usingState . modify . set
    where set heap (scope, _) = (scope, heap)

{- Value -}
instance Show Value where
    show Vnothing       = "Nothing"
    show (Vi int)       = show int
    show (Vb bool)      = show bool
    show (Vs string)    = string
    show (Vrl index)    = "LIST REF: " ++ (show index) ++ ""
    show (Vl list)      = show (Fold.toList list)
    show (Vsf _ name)   = name ++ "(..)"
    show (Vbsf _ i name) = (show i) ++ ":" ++ name ++ "(..)"

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

newRuntime :: Scope -> StateData
newRuntime scope = (scope, newMemory)
