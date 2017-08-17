module State (
    Failure(..),
    Signal(..),
    Value(..),
    Runtime,

    getVar,
    setVar,

    getStackHeight,
    pushScope,
    popScope,

    addToHeap,
    setAtHeap,
    getFromHeap,

    usingIO,
    isEOF,
    readLine,
    writeLine,
    run,
    newRuntime,

    throwSignal,
    catchSignal,
    tokenizeFail,
    parseFail,
    evalFail,
    typeFail1,
    typeFail2,
    argFail,
) where

import Data.List (intercalate)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import qualified System.IO as IO
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

import Memory
import Decls(Line)

type Scope          = Map.Map String Value
type Stack          = [Scope]
type Heap           = Memory Value
type StateData      = (IO.Handle, Scope, Stack, Heap)
type RuntimeState   = StateT StateData IO
type Runtime        = ExceptT Signal RuntimeState

type SysFunc        = [Value] -> Runtime Value
type BoundSysFunc   = Int -> [Value] -> Runtime Value

{- Signal -}
data Signal = Sreturn Value
            | Sfailure Failure
            deriving (Show)

{- Failure -}
data Failure = Tokenize String
             | Parse String
             | Eval String

instance Show Failure where
    show (Tokenize message) = "<tokenize> " ++ message
    show (Parse message)    = "<parse> " ++ message
    show (Eval message)     = "<eval> " ++ message

{- Value -}
data Value = Vnothing
           | Vi Int
           | Vb Bool
           | Vs String
           | Vl (Seq.Seq Value)
           | Vrl Int
           | Vsf SysFunc String
           | Vbsf BoundSysFunc Int String
           | Vuf [String] Line String
           | Vbuf [String] Line Int String

instance Show Value where
    show Vnothing       = "Nothing"
    show (Vi int)       = show int
    show (Vb bool)      = show bool
    show (Vs string)    = string
    show (Vrl index)    = "LIST REF: " ++ (show index) ++ ""
    show (Vl list)      = show (Fold.toList list)
    show (Vsf _ name)   = name ++ "(..)"
    show (Vbsf _ i name) = (show i) ++ ":" ++ name ++ "(..)"
    show (Vuf args _ name)   = name ++ "(" ++ (intercalate ", " args) ++ ")"
    show (Vbuf args _ i name) = (show i) ++ ":" ++ name ++ "(" ++ (intercalate ", " args) ++ ")"

{- State Modifiers -}
setVar :: String -> Value -> Runtime ()
setVar name val = do (input, globals, stack, heap) <- usingState get
                     let (newGlobals, newStack) = updateEither globals stack
                         newHeap = if numInserts heap > 5
                                   then gc heap newGlobals newStack
                                   else heap
                     usingState . put $ (input, newGlobals, newStack, newHeap)
    where updateEither globals (top:rest) = (globals, ((setScope top):rest))
          updateEither globals [] = (setScope globals, [])
          setScope = Map.insert name val

getVar ::  String -> Runtime Value
getVar name = usingState get >>= find
    where find (_, globals, [], _) = toVal (Map.lookup name globals)
          find (_, globals, (top:_), _) = toValOr (Map.lookup name top) globals
          toValOr (Just val) _       = return val;
          toValOr Nothing    globals = toVal (Map.lookup name globals)
          toVal (Just val)           = return val
          toVal Nothing              = evalFail $ "variable " ++ (show name)
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
    where onlyInput (input, _, _, _) = return input

getStackHeight :: Runtime Int
getStackHeight = usingState get >>= len
    where len (_, _, stack, _) = return (length stack)

popScope :: Runtime ()
popScope = usingState . modify $ pop
    where pop (input, globals, (_:stack), heap) = (input, globals, stack, heap)
          pop (_, _, _, _)  = error "poping empty stack"

pushScope :: Scope -> Runtime ()
pushScope = usingState . modify . push
    where push newScope (input, globals, stack, heap) =
                        (input, globals, (newScope:stack), heap)

getHeap :: Runtime Heap
getHeap = usingState get >>= onlyHeap
    where onlyHeap (_, _, _, heap) = return heap

setHeap :: Heap -> Runtime ()
setHeap = usingState . modify . set
    where set heap (input, globals, stack, _) = (input, globals, stack, heap)

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

run :: Runtime a -> StateData -> IO (Either Signal a)
run = evalStateT . runExceptT

newRuntime :: IO.Handle -> Scope -> StateData
newRuntime input scope = (input, scope, [], newMemory)

{- Garbage Collection -}
gc :: Heap -> Scope -> Stack -> Heap
gc (Memory items _ _) globals stack = Memory newItems newFreeIndices 0
    where (newItems, _) = trimmed
          trimmed = Seq.foldrWithIndex (trim refSet) (Seq.empty, True) nulled
          nulled = Seq.mapWithIndex (nullify refSet) items
          newFreeIndices = Set.fromList (take newSize [0..]) Set.\\ refSet
          newSize = Seq.length newItems
          refSet = foldr traceScope (Set.empty) (globals:stack)
          traceScope :: Scope -> Set.Set Int -> Set.Set Int
          traceScope scope refs = Map.foldr (traceVar items) refs scope

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

{- Signals -}
throwSignal :: Signal -> Runtime a
throwSignal = throwE

catchSignal :: Runtime a -> (Signal -> Runtime a) -> Runtime a
catchSignal = catchE

{- Failure -}
tokenizeFail :: String -> Runtime a
tokenizeFail = throwE . Sfailure . Tokenize

parseFail ::  String -> Runtime a
parseFail = throwE . Sfailure . Parse

evalFail :: String -> Runtime a
evalFail = throwE . Sfailure . Eval

typeFail1 :: (Show a) => String -> a -> Runtime b
typeFail1 opName x   = evalFail $ opName ++ " of \"" ++ (show x) ++ "\" "
                                ++ "is not supported"

typeFail2 :: (Show a) => String -> a -> a -> Runtime b
typeFail2 opName x y = evalFail $ opName ++ " of \""
                              ++ (show x) ++ "\" and \"" ++ (show y) ++ "\" "
                              ++ "is not supported"

argFail :: String -> Runtime a
argFail expected = evalFail $ "unexpected arguments, expected " ++ expected
