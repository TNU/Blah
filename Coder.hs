module Coder (
    Value(..),
    repl
) where

import Control.Monad.Instances
import Tokenizer
import Parser

data Value = Vi Integer
           deriving (Show)

type Failable = Either String

main = interact repl

repl :: String -> String
repl = eval . replParse . tokenize

replParse :: Monad m => [Failable [Token]] -> [m Decl]
replParse = replParseLine []

replParseLine :: Monad m => [Decl] -> [Failable [Token]] -> [m Decl]
replParseLine decls (tokens:rest) = case tokens >>= parse decls of
        Right [decl@(De _)] -> (return decl):(replParseLine [] rest)
        Right newDecls      -> replParseLine newDecls rest
        Left message        -> (fail message):(replParseLine [] rest)
replParseLine (x:xs) [] = [fail $ "Repl: insufficient decls" ++ " " ++ show (xs)]
replParseLine []     [] = []
        
eval :: [Failable Decl] -> String
eval = unlines . map show