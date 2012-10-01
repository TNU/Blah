module Coder (
    Value(..),
    repl
) where

import Control.Monad.Error

import Tokenizer
import Parser
           
type Failable = Either String

data Value = Vi Integer
           deriving (Show)

main = interact repl

repl :: String -> String
repl = eval . replParse [] . tokenize

replParse :: (Error e, MonadError e m) => [Decl] -> [Failable [Token]] -> [m Decl]
replParse decls (tokens:rest) = case tokens >>= parse decls of
        Right [decl@(De _)] -> (return decl):(replParse [] rest)
        Right newDecls      -> replParse newDecls rest
        Left message        -> (passFailMsg message):(replParse [] rest)
replParse (x:xs) [] = [replFail $ "insufficient decls" ++ " " ++ show (xs)]
replParse []     [] = []

eval :: [Failable Decl] -> String
eval = unlines . map show

-- error handling
replFail :: (Error e, MonadError e m) => String -> m a
replFail = throwError . strMsg . ("[repl] " ++)

passFailMsg :: (Error e, MonadError e m) => String -> m a
passFailMsg = throwError . strMsg
