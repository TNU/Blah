module State (
    Value(..),
    Runtime,

    getVar,
    setVar,
    addToHeap,
    setAtHeap,
    getFromHeap,

    usingIO,
    isEOF,
    readLine,
    writeLine,
    run,
    newRuntime,
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Error

import qualified System.IO as IO
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

import Failure
import Memory

type Scope          = Map.Map String Value
type Heap           = Memory Value
type StateData      = (IO.Handle, Scope, Heap)
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
setVar name val = do (input, scope, heap) <- usingState get
                     let newScope = Map.insert name val scope
                         newHeap = if numInserts heap > 5
                                   then gc heap newScope
                                   else heap
                     usingState . put $ (input, newScope, newHeap)

getVar ::  String -> Runtime Value
getVar name = usingState get >>= find
    where find (_, scope, _) = toVal (Map.lookup name scope)
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

getInput :: Runtime IO.Handle
getInput = usingState get >>= onlyInput
    where onlyInput (input, _, _) = return input

getHeap :: Runtime Heap
getHeap = usingState get >>= onlyHeap
    where onlyHeap (_, _, heap) = return heap

setHeap :: Heap -> Runtime ()
setHeap = usingState . modify . set
    where set heap (input, scope, _) = (input, scope, heap)

{- IO Operations -}
readLine :: Runtime String
readLine = getInput >>= usingIO . IO.hGetLine

isEOF :: Runtime Bool
isEOF = getInput >>= usingIO . IO.hIsEOF

writeLine :: String -> Runtime ()
writeLine = usingIO . putStrLn

usingState :: RuntimeState a -> Runtime a
usingState = lift

usingIO :: IO a -> Runtime a
usingIO = lift . lift

run :: Runtime a -> StateData -> IO (Either Failure a)
run = evalStateT . runErrorT

newRuntime :: IO.Handle -> Scope -> StateData
newRuntime input scope = (input, scope, newMemory)

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

{- Garbage Collection -}
gc :: Heap -> Scope -> Heap
gc (Memory items freeIndices _) scope = Memory newItems newFreeIndices 0
    where refSet = Map.foldr (traceVar items) freeIndices scope
          nulled = Seq.mapWithIndex (nullify refSet) items
          trimmed = Seq.foldrWithIndex (trim refSet) (Seq.empty, True) nulled
          (newItems, _) = trimmed
          newSize = Seq.length newItems
          newFreeIndices = Set.fromList (take newSize [0..]) Set.\\ refSet

traceVar :: Seq.Seq Value -> Value -> Set.Set Int -> Set.Set Int
traceVar index (Vrl i)      seen = trace index i seen
traceVar index (Vbsf _ i _) seen = trace index i seen
traceVar index (Vl list)    seen = Fold.foldr (traceVar index) seen list
traceVar _     _            seen = seen

trace :: Seq.Seq Value -> Int -> Set.Set Int -> Set.Set Int
trace index i seen
    | Set.member i seen = seen
    | otherwise = traceVar index (Seq.index index i) (Set.insert i seen)

nullify :: Set.Set Int -> Int -> Value -> Value
nullify refSet index val = if Set.member index refSet then val else Vnothing

trim :: Set.Set Int -> Int -> Value
                    -> (Seq.Seq Value, Bool) -> (Seq.Seq Value, Bool)
trim refSet index value (items, True)
    | Set.member index refSet = (value Seq.<| items, False)
    | otherwise               = (items, True)
trim _      _     value (items, _) = (value Seq.<| items, False)
