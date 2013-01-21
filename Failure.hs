module Failure (
    Failable,
    Failure,

    tokenizeFail,
    parseFail,
    evalFail,
    typeFail1,
    typeFail2,
    argFail,
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

typeFail1 :: (Monad m, Show a) => String -> a -> ErrorT Failure m b
typeFail1 opName x   = evalFail $ opName ++ " of \"" ++ (show x) ++ "\" "
                                ++ "is not supported"

typeFail2 :: (Monad m, Show a) => String -> a -> a -> ErrorT Failure m b
typeFail2 opName x y = evalFail $ opName ++ " of \""
                              ++ (show x) ++ "\" and \"" ++ (show y) ++ "\" "
                              ++ "is not supported"

argFail :: (Monad m) => String -> ErrorT Failure m a
argFail expected = evalFail $ "unexpected arguments, expected " ++ expected

