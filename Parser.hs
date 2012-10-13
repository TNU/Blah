module Parser (
    Stmt(..),
    Decl(..),
    Expr(..),
    Term(..),
    Factor(..),
    Neg(..),
    Paren(..),
    parse,
) where

import Control.Monad.Error

import Tokenizer

data Decl = De Expr
          | Dt Term
          | Df Factor
          | Dk Token
          | Ds Stmt
          | Dn Neg
          deriving (Show, Eq)

data Stmt = Assn String Expr
          deriving (Show, Eq)
          
data Expr = Et Term
          | Add Expr Term
          | Sub Expr Term
          deriving (Show, Eq)

data Term = Tf Factor
          | Mult Term Factor
          | Div Term Factor
          deriving (Show, Eq)
          
data Factor = Fp Neg
            | Fn Neg
            deriving (Show, Eq)

data Neg  = Ni Integer
          | Nd String
          | Np Paren
          deriving (Show, Eq)

data Paren = Pe Expr
           deriving (Show, Eq)

parse :: (Error e, MonadError e m) => [Decl] -> [Token] -> m [Decl]
parse = shift

shift :: (Error e, MonadError e m) => [Decl] -> [Token] -> m [Decl]
shift decls (tok:tokens)        = reduce ((Dk tok):decls) tokens 
shift decls []                  = return decls

reduce :: (Error e, MonadError e m) => [Decl] -> [Token] -> m [Decl]

{- terms -}
reduce decls@((Dt term):rest) tokens@(tok:_) | isTermOp tok = shift decls tokens
reduce decls@((Dk MULT):(Dt term):rest)      tokens = shift decls tokens
reduce decls@((Dk DIV):(Dt term):rest)       tokens = shift decls tokens
reduce ((Df fact):(Dk MULT):(Dt term):rest)  tokens = reduce ((Dt (Mult term fact)):rest) tokens
reduce ((Df fact):(Dk DIV):(Dt term):rest)   tokens = reduce ((Dt (Div term fact)):rest)  tokens

{- expr -}
reduce decls@((De expr):rest) tokens@(tok:_) | isExprOp tok = shift decls tokens
reduce decls@((Dk PLUS):(De expr):rest)      tokens = shift decls tokens
reduce decls@((Dk MINUS):(De expr):rest)     tokens = shift decls tokens
reduce ((Dt term):(Dk PLUS):(De expr):rest)  tokens = reduce ((De (Add expr term)):rest)  tokens
reduce ((Dt term):(Dk MINUS):(De expr):rest) tokens = reduce ((De (Sub expr term)):rest)  tokens

{- parentheses -}
reduce decls@((Dk LPAREN):rest)                 tokens = shift decls tokens
reduce decls@((De expr):(Dk LPAREN):rest)       tokens = shift decls tokens
reduce ((Dk RPAREN):(De expr):(Dk LPAREN):rest) tokens = reduce ((Dn (Np (Pe expr))):rest) tokens

{- assign -}  -- assignments not allowed across lines
reduce decls@((Dn (Nd id)):rest)       tokens@(ASSN:_) = shift decls tokens
reduce decls@((Dk ASSN):(Dn (Nd id)):rs)  tokens@(_:_) = shift decls tokens
reduce ((De expr):(Dk ASSN):(Dn (Nd name)):rs)  tokens = reduce ((Ds (Assn name expr)):rs) tokens

{- negation -}  -- negation not allowed across lines
reduce decls@((Dk MINUS):rest) tokens@(tok:_)          = shift decls tokens
reduce ((Dn neg):(Dk MINUS):rest) tokens
    | isNegatable rest                                 = reduce (Df (Fn neg):rest) tokens

{- promotions -}
reduce ((Dk (INT int)):rest)    tokens = reduce ((Dn (Ni int)):rest)  tokens
reduce ((Dk (ID id)):rest)      tokens = reduce ((Dn (Nd id)):rest)   tokens
reduce ((Dn neg):rest)          tokens = reduce ((Df (Fp neg)):rest)  tokens
reduce ((Df fact):rest)         tokens = reduce ((Dt (Tf fact)):rest) tokens
reduce ((Dt term):rest)         tokens = reduce ((De (Et term)):rest) tokens

{- base cases -}
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

parseTest = (p "1 + 2" == Right [De (Add (Et (Tf (Fp (Ni 1)))) (Tf (Fp (Ni 2))))])
         && (p "1 * 2" == Right [De (Et (Mult (Tf (Fp (Ni 1))) (Fp (Ni 2))))])
         && (p "1 + - 2 --2" == Right [De (Sub (Add (Et (Tf (Fp (Ni 1)))) (Tf (Fn (Ni 2)))) (Tf (Fn (Ni 2))))])
         && (p "1 + 2 * 3 * -3 - -3 + 4 * 5 / 6" == 
            Right [De (Add (Sub (Add (Et (Tf (Fp (Ni 1)))) (Mult (Mult (Tf (Fp (Ni 2))) (Fp (Ni 3))) (Fn (Ni 3)))) (Tf (Fn (Ni 3)))) (Div (Mult (Tf (Fp (Ni 4))) (Fp (Ni 5))) (Fp (Ni 6))))])
         && (p "1 - 2" == Right [De (Sub (Et (Tf (Fp (Ni 1)))) (Tf (Fp (Ni 2))))])
         && (p "- 1" == Right [De (Et (Tf (Fn (Ni 1))))])
         && (p "-(1 * 1)" == Right [De (Et (Tf (Fn (Np (Pe (Et (Mult (Tf (Fp (Ni 1))) (Fp (Ni 1)))))))))])
         && (p "-(-(-(-1)))" == Right [De (Et (Tf (Fn (Np (Pe (Et (Tf (Fn (Np (Pe (Et (Tf (Fn (Np (Pe (Et (Tf (Fn (Ni 1)))))))))))))))))))])
         && (p "-1 * -(2 + 3 * 4)" == Right [De (Et (Mult (Tf (Fn (Ni 1))) (Fn (Np (Pe (Add (Et (Tf (Fp (Ni 2)))) (Mult (Tf (Fp (Ni 3))) (Fp (Ni 4)))))))))])
         && (p "abc: 1 - 3" == Right [Ds (Assn "abc" (Sub (Et (Tf (Fp (Ni 1)))) (Tf (Fp (Ni 3)))))])
         && (p "abc: def" == Right [Ds (Assn "abc" (Et (Tf (Fp (Nd "def")))))])
         && (p "a: (b - c / 2) * d" == 
            Right [Ds (Assn "a" (Et (Mult (Tf (Fp (Np (Pe (Sub (Et (Tf (Fp (Nd "b")))) (Div (Tf (Fp (Nd "c"))) (Fp (Ni 2)))))))) (Fp (Nd "d")))))])
         && (p "a\n:" == Right [De (Et (Tf (Fp (Nd "a"))))]) -- not assign
         
         && (p "1 + - - \n 1" == Left "<parse> [Dk MINUS,Dk MINUS,Dk PLUS,De (Et (Tf (Fp (Ni 1))))] []")
         && (p "-- 1" == Left "<parse> [De (Et (Tf (Fn (Ni 1)))),Dk MINUS] []")
         && (p "a:" == Left "<parse> [Dk ASSN,Dn (Nd \"a\")] []")
         && (p "a+b: 3" == Left "<parse> [Ds (Assn \"b\" (Et (Tf (Fp (Ni 3))))),Dk PLUS,De (Et (Tf (Fp (Nd \"a\"))))] []")
         where p = parseStr :: String -> Either String [Decl]