module Parser (
    Decl(..),
    parse
) where

import Control.Monad.Error
import Data.List (findIndex)

import Tokenizer

data Decl = De Expr
          | Dt Term
          | Df Factor
          | Dk Token
          deriving (Show, Eq)

data Expr = Et Term
          | Add Expr Term
          | Sub Expr Term
          deriving (Show, Eq)

data Term = Tf Factor
          | Mult Term Factor
          | Div Term Factor
          deriving (Show, Eq)
          
data Factor = Fi Integer
            | Fn Neg
            deriving (Show, Eq)

data Neg  = Ni Integer
          deriving (Show, Eq)

parse :: (Error e, MonadError e m) => [Decl] -> [Token] -> m [Decl]
parse = shift


shift :: (Error e, MonadError e m) => [Decl] -> [Token] -> m [Decl]
shift decls ((INT int):tokens)  = reduce ((Df (Fi int)):decls)  tokens
shift decls (tok:tokens)        = reduce ((Dk tok):decls) tokens 
shift decls []                  = return decls


reduce :: (Error e, MonadError e m) => [Decl] -> [Token] -> m [Decl]

-- plus, minus order shifts
reduce decls@((Dt term):(Dk PLUS):(De expr):rest) tokens@(tok:_)
    | isTermOp tok  = shift decls tokens 
reduce decls@((Dt term):(Dk MINUS):(De expr):rest) tokens@(tok:_)
    | isTermOp tok  = shift decls tokens
    
-- plus, minus, mult, divide reductions
reduce ((Dt term):(Dk PLUS):(De expr):rest)  tokens = reduce ((De (Add expr term)):rest) tokens
reduce ((Dt term):(Dk MINUS):(De expr):rest) tokens = reduce ((De (Sub expr term)):rest) tokens
reduce ((Df fact):(Dk MULT):(Dt term):rest)  tokens = reduce ((Dt (Mult term fact)):rest) tokens
reduce ((Df fact):(Dk DIV):(Dt term):rest)   tokens = reduce ((Dt (Div term fact)):rest) tokens
    
-- plus, minus, mult, divide shifts
reduce decls@((Dk PLUS):(De expr):rest)  tokens = shift decls tokens
reduce decls@((Dk MINUS):(De expr):rest) tokens = shift decls tokens
reduce decls@((Dk MULT):(Dt term):rest)  tokens = shift decls tokens
reduce decls@((Dk DIV):(Dt term):rest)   tokens = shift decls tokens

-- operation shifts
reduce decls@((De expr):rest) tokens@(tok:_) 
    | isExprOp tok  = shift decls tokens
reduce decls@((Dt term):rest) tokens@(tok:_) 
    | isTermOp tok = shift decls tokens

-- negation reduction
reduce ((Df (Fi int)):(Dk MINUS):rest) tokens = reduce (Df (Fn (Ni int)):rest) tokens

-- negation shift
reduce decls@((Dk MINUS):rest) tokens@(tok:_) 
    | isNegatable tok = shift decls tokens
-- (negation across lines are not allowed)

-- promotional reductions
reduce ((Df fact):rest) tokens = reduce ((Dt (Tf fact)):rest) tokens
reduce ((Dt term):rest) tokens = reduce ((De (Et term)):rest) tokens

reduce decls@[_]    []      = return decls
reduce decls        tokens  = throwError . strMsg $ 
                        "PARSE: " ++ (show decls) ++ " " ++ (show tokens)

            
isExprOp tok        = tok `elem` [PLUS, MINUS]
isTermOp tok        = tok `elem` [MULT, DIV]

isNegatable (INT _) = True
isNegatable _       = False
          
parseStr :: String -> Either String [Decl]
parseStr str = head (tokenize str) >>= parse []

parseTest = (parseStr "1 + 2" == Right [De (Add (Et (Tf (Fi 1))) (Tf (Fi 2)))])
         && (parseStr "1 * 2" == Right [De (Et (Mult (Tf (Fi 1)) (Fi 2)))])
         && (parseStr "1 + - 2 --2" == Right [De (Sub (Add (Et (Tf (Fi 1))) (Tf (Fn (Ni 2)))) (Tf (Fn (Ni 2))))])
         && (parseStr "1 + 2 * 3 * -3 - -3 + 4 * 5 / 6" == 
             Right [De (Add (Sub (Add (Et (Tf (Fi 1))) (Mult (Mult (Tf (Fi 2)) (Fi 3)) (Fn (Ni 3)))) (Tf (Fn (Ni 3)))) (Div (Mult (Tf (Fi 4)) (Fi 5)) (Fi 6)))])