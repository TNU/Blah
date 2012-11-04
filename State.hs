module State (
    Failure(..),
    tokenizeFail,
    parseFail,
    evalFail,

    Runtime(..),
    Scope(..),
    getVar,
    setVar,
    isEOF,
    readChar,
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

import Value

type Failure        = String
type Failable m     = ErrorT Failure m

type Scope          = Map.Map String Value
type StateData      = Scope
type RuntimeState   = StateT StateData IO
type Runtime        = Failable RuntimeState

{- State Modifiers -}
getVar ::  String -> Runtime Value
getVar name = usingState get >>= find
   where find scope = toVal (Map.lookup name scope)
         toVal (Just val)  = return val
         toVal Nothing     = evalFail  $ "variable " ++ (show name)
                                      ++ " does not exist"

setVar :: String -> Value -> Runtime ()
setVar name val = usingState . modify $ Map.insert name val

readChar :: Runtime Char
readChar = usingIO getChar

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
newRuntime scope = scope

tokenizeFail :: String -> Runtime a
tokenizeFail = throwError . strMsg . ("<tokenize> " ++)

parseFail :: String -> Runtime a
parseFail = throwError . strMsg . ("<parse> " ++)

evalFail :: String -> Runtime a
evalFail = throwError . strMsg . ("<eval> " ++)
