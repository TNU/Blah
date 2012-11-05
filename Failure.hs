module Failure (
    Failure(..),
    Failable(..),

    tokenizeFail,
    parseFail,
    evalFail,
) where

import Control.Monad.Trans.Error

type Failure        = String
type Failable m     = ErrorT Failure m

tokenizeFail :: (Monad m) => String -> ErrorT Failure m a
tokenizeFail = throwError . strMsg . ("<tokenize> " ++)

parseFail :: (Monad m) => String -> ErrorT Failure m a
parseFail = throwError . strMsg . ("<parse> " ++)

evalFail :: (Monad m) => String -> ErrorT Failure m a
evalFail = throwError . strMsg . ("<eval> " ++)
