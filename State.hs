module State (
    Runtime(..),
    Scope(..),
    SystemFuncMap(..),
    getVar,
    setVar,
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

import Failure
import Value
import Func

type Scope          = Map.Map String Value
type StateData      = (Scope, SystemFuncMap)
type RuntimeState   = StateT StateData IO
type Runtime        = Failable RuntimeState

{- State Modifiers -}
getVar ::  String -> Runtime Value
getVar name = usingState get >>= find
   where find (scope, _)  = toVal (Map.lookup name scope)
         toVal (Just val) = return val
         toVal Nothing    = evalFail  $ "variable " ++ (show name)
                                     ++ " does not exist"

setVar :: String -> Value -> Runtime ()
setVar name val = usingState . modify $ set
    where set (scope, sysFuncs) = (Map.insert name val scope, sysFuncs)

getSysFunc :: String -> Runtime SystemFunc
getSysFunc name = usingState get >>= find
    where find (_, sysFuncs) = toVal (Map.lookup name sysFuncs)
          toVal (Just func)  = return func
          toVal Nothing      = evalFail $ "system function " ++ (show name)
                                       ++ " does not exist"

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
newRuntime scope sysFuncs = (scope, sysFuncs)
