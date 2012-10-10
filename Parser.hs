module Parser (
    Decl(..),
    Expr(..),
    Term(..),
    Factor(..),
    Neg(..),
    Paren(..),
    parse
) where

import Control.Monad.Error

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
            | Fp Paren
            deriving (Show, Eq)

data Neg  = Ni Integer
          | Np Paren
          deriving (Show, Eq)

data Paren = Pe Expr
           deriving (Show, Eq)

parse :: (Error e, MonadError e m) => [Decl] -> [Token] -> m [Decl]
parse = shift

shift :: (Error e, MonadError e m) => [Decl] -> [Token] -> m [Decl]
shift decls ((INT int):tokens)  = reduce ((Df (Fi int)):decls)  tokens
shift decls (tok:tokens)        = reduce ((Dk tok):decls) tokens 
shift decls []                  = return decls


reduce :: (Error e, MonadError e m) => [Decl] -> [Token] -> m [Decl]

{- Three Levels -}
-- plus, minus order shifts
reduce decls@((Dt term):(Dk PLUS):(De expr):rest) tokens@(tok:_)
    | isTermOp tok  = shift decls tokens 
reduce decls@((Dt term):(Dk MINUS):(De expr):rest) tokens@(tok:_)
    | isTermOp tok  = shift decls tokens
    
-- plus, minus, mult, divide reductions
reduce ((Dt term):(Dk PLUS):(De expr):rest)  tokens = reduce ((De (Add expr term)):rest)  tokens
reduce ((Dt term):(Dk MINUS):(De expr):rest) tokens = reduce ((De (Sub expr term)):rest)  tokens
reduce ((Df fact):(Dk MULT):(Dt term):rest)  tokens = reduce ((Dt (Mult term fact)):rest) tokens
reduce ((Df fact):(Dk DIV):(Dt term):rest)   tokens = reduce ((Dt (Div term fact)):rest)  tokens
    
-- parenthsis reduction
reduce ((Dk RPAREN):(De expr):(Dk LPAREN):rest) tokens = reduce ((Df (Fp (Pe expr))):rest) tokens
    
{- Two Levels -}    
-- plus, minus, mult, divide shifts
reduce decls@((Dk PLUS):(De expr):rest)      tokens = shift decls tokens
reduce decls@((Dk MINUS):(De expr):rest)     tokens = shift decls tokens
reduce decls@((Dk MULT):(Dt term):rest)      tokens = shift decls tokens
reduce decls@((Dk DIV):(Dt term):rest)       tokens = shift decls tokens

-- operation shifts
reduce decls@((De expr):rest) tokens@(tok:_) 
    | isExprOp tok = shift decls tokens
reduce decls@((Dt term):rest) tokens@(tok:_) 
    | isTermOp tok = shift decls tokens

-- negation reduction
reduce ((Df (Fi int)):(Dk MINUS):rest) tokens
    | isNegatable rest = reduce (Df (Fn (Ni int)):rest) tokens
reduce ((Df (Fp paren)):(Dk MINUS):rest) tokens
    | isNegatable rest = reduce (Df (Fn (Np paren)):rest) tokens

-- parenthsis reduction
reduce decls@((De expr):(Dk LPAREN):rest)    tokens = shift decls tokens
    
{- One Level -}
-- negation shift
-- negation across lines are not allowed
reduce decls@((Dk MINUS):rest) tokens@(tok:_) = shift decls tokens

-- parenthsis shift
reduce decls@((Dk LPAREN):rest) tokens = shift decls tokens

-- promotional reductions
reduce ((Df fact):rest) tokens = reduce ((Dt (Tf fact)):rest) tokens
reduce ((Dt term):rest) tokens = reduce ((De (Et term)):rest) tokens

-- base cases
reduce decls@[_]    []      = return decls
reduce decls        tokens  = parseFail $ (show decls) ++ " " ++ (show tokens)

            
isExprOp tok        = tok `elem` [PLUS, MINUS]
isTermOp tok        = tok `elem` [MULT, DIV]

isNegatable ((De _):xs) = False
isNegatable _           = True

-- error handling
parseFail :: (Error e, MonadError e m) => String -> m a
parseFail = throwError . strMsg . ("<parse> " ++)
          
-- testing
parseStr :: (Error e, MonadError e m) => String -> m [Decl]
parseStr str = fst (tokenize str) >>= parse []

parseTest = (p "1 + 2" == Right [De (Add (Et (Tf (Fi 1))) (Tf (Fi 2)))])
         && (p "1 * 2" == Right [De (Et (Mult (Tf (Fi 1)) (Fi 2)))])
         && (p "1 + - 2 --2" == Right [De (Sub (Add (Et (Tf (Fi 1))) (Tf (Fn (Ni 2)))) (Tf (Fn (Ni 2))))])
         && (p "1 + 2 * 3 * -3 - -3 + 4 * 5 / 6" == 
             Right [De (Add (Sub (Add (Et (Tf (Fi 1))) (Mult (Mult (Tf (Fi 2)) (Fi 3)) (Fn (Ni 3)))) (Tf (Fn (Ni 3)))) (Div (Mult (Tf (Fi 4)) (Fi 5)) (Fi 6)))])
         && (p "1 - 2" == Right [De (Sub (Et (Tf (Fi 1))) (Tf (Fi 2)))])
         && (p "- 1" == Right [De (Et (Tf (Fn (Ni 1))))])
         && (p "-(1 * 1)" == Right [De (Et (Tf (Fn (Np (Pe (Et (Mult (Tf (Fi 1)) (Fi 1))))))))])
         && (p "-(-(-(-1)))" == Right [De (Et (Tf (Fn (Np (Pe (Et (Tf (Fn (Np (Pe (Et (Tf (Fn (Np (Pe (Et (Tf (Fn (Ni 1)))))))))))))))))))])
         && (p "-1 * -(2 + 3 * 4)" == Right [De (Et (Mult (Tf (Fn (Ni 1))) (Fn (Np (Pe (Add (Et (Tf (Fi 2))) (Mult (Tf (Fi 3)) (Fi 4))))))))])
         
         && (p "1 + - - \n 1" == Left "<parse> [Dk MINUS,Dk MINUS,Dk PLUS,De (Et (Tf (Fi 1)))] []")
         && (p "-- 1" == Left "<parse> [De (Et (Tf (Fn (Ni 1)))),Dk MINUS] []")
         
         where p = parseStr :: String -> Either String [Decl]