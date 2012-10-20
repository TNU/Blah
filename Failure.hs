module Failure (
    Failure(..),
    Failable(..),
    FailableM(..),
    tokenizeFail,
    parseFail,
    evalError,
    evalFail,
    replError,
    extract,
) where

import Control.Monad.Trans.Error
import Data.Functor.Identity

type Failure     = String
type Failable a  = ErrorT Failure Identity a
type FailableM m = ErrorT Failure m

tokenizeFail :: String -> Failable a
tokenizeFail = throwError . strMsg . ("<tokenize> " ++)

parseFail :: String -> Failable a
parseFail = throwError . strMsg . ("<parse> " ++)

evalError :: (Error e) => String -> e
evalError = strMsg . ("<eval> " ++)

evalFail :: (Monad m) => String -> FailableM m a
evalFail = throwError . evalError

replError:: String -> IO ()
replError = putStrLn . ("<repl> " ++)

extract :: Failable a -> Either Failure a
extract = runIdentity . runErrorT
