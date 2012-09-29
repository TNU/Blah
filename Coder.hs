module Coder (
    Value(..),
    repl
) where

import Control.Monad.Error

import Tokenizer
import Parser

data Value = Vi Integer
           deriving (Show)

type Failable = Either String

main = interact repl

repl :: String -> String
repl = eval . replParse [] . tokenize

replParse :: (Error e, MonadError e m) => [Decl] -> [Failable [Token]] -> [m Decl]
replParse decls (tokens:rest) = case tokens >>= parse decls of
        Right [decl@(De _)] -> (return decl):(replParse [] rest)
        Right newDecls      -> replParse newDecls rest
        Left message        -> (throwError . strMsg $ message):(replParse [] rest)
replParse (x:xs) [] = [throwError . strMsg $ "Repl: insufficient decls" ++ " " ++ show (xs)]
replParse []     [] = []
        
eval :: [Failable Decl] -> String
eval = unlines . map show