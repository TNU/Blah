module Parser (
    Stmt(..),
    Decl(..),
    OrOp(..),
    AndOp(..),
    Comp(..),
    Arth(..),
    Term(..),
    Factor(..),
    Neg(..),
    Paren(..),
    parse,
) where

import Data.Functor.Identity
import Control.Monad.Trans.Error

import Failure
import Tokenizer

data Decl = Ds Stmt
          | Do OrOp
          | Dd AndOp
          | Dc Comp
          | Da Arth
          | Dt Term
          | Df Factor
          | Dk Token
          | Dn Neg
          deriving (Show, Eq)

data Stmt = So OrOp
          | Assn String OrOp
          deriving (Show, Eq)

data OrOp = Oa AndOp
          | Or OrOp AndOp
          deriving (Show, Eq)

data AndOp = Ac Comp
          | And AndOp Comp
          deriving (Show, Eq)

data Comp = Ca Arth
          | Lt  Comp Arth
          | Eq  Comp Arth
          | Gt  Comp Arth
          | Lte Comp Arth
          | Gte Comp Arth
          | Ne  Comp Arth
          deriving (Show, Eq)

data Arth = At Term
          | Add Arth Term
          | Sub Arth Term
          deriving (Show, Eq)

data Term = Tf Factor
          | Mult Term Factor
          | Div Term Factor
          deriving (Show, Eq)

data Factor = Fp Neg
            | Fn Neg
            | Fb Bool
            deriving (Show, Eq)

data Neg  = Ni Integer
          | Nd String
          | Np Paren
          deriving (Show, Eq)

data Paren = Po OrOp
           deriving (Show, Eq)

parse :: [Decl] -> [Token] -> Failable [Decl]
parse = shift

shift :: [Decl] -> [Token] -> Failable [Decl]
shift decls (tok:tokens)        = reduce ((Dk tok):decls) tokens
shift decls []                  = return decls

reduce :: [Decl] -> [Token] -> Failable [Decl]

{- terms -}
reduce decls@((Dt term):rest)       tokens@(MULT:_) = shift decls tokens
reduce decls@((Dt term):rest)       tokens@(DIV:_)  = shift decls tokens
reduce decls@((Dk MULT):(Dt term):rest)      tokens = shift decls tokens
reduce decls@((Dk DIV):(Dt term):rest)       tokens = shift decls tokens
reduce ((Df fact):(Dk MULT):(Dt term):rest)  tokens = reduce ((Dt (Mult term fact)):rest) tokens
reduce ((Df fact):(Dk DIV):(Dt term):rest)   tokens = reduce ((Dt (Div term fact)):rest)  tokens

{- arithmetics -}
reduce decls@((Da orOp):rest)      tokens@(PLUS:_)  = shift decls tokens
reduce decls@((Da arth):rest)      tokens@(MINUS:_) = shift decls tokens
reduce decls@((Dk PLUS):(Da arth):rest)      tokens = shift decls tokens
reduce decls@((Dk MINUS):(Da arth):rest)     tokens = shift decls tokens
reduce ((Dt term):(Dk PLUS):(Da arth):rest)  tokens = reduce ((Da (Add arth term)):rest)  tokens
reduce ((Dt term):(Dk MINUS):(Da arth):rest) tokens = reduce ((Da (Sub arth term)):rest)  tokens

{- comparisons -}
reduce decls@((Dc comp):rest)      tokens@(TLTE:_) = shift decls tokens
reduce decls@((Dc comp):rest)      tokens@(TGTE:_) = shift decls tokens
reduce decls@((Dc comp):rest)      tokens@(TNE:_)  = shift decls tokens
reduce decls@((Dc comp):rest)      tokens@(TLT:_)  = shift decls tokens
reduce decls@((Dc comp):rest)      tokens@(TEQ:_)  = shift decls tokens
reduce decls@((Dc comp):rest)      tokens@(TGT:_)  = shift decls tokens
reduce decls@((Dk TLTE):(Dc comp):rest)     tokens = shift decls tokens
reduce decls@((Dk TGTE):(Dc comp):rest)     tokens = shift decls tokens
reduce decls@((Dk TNE):(Dc comp):rest)      tokens = shift decls tokens
reduce decls@((Dk TLT):(Dc comp):rest)      tokens = shift decls tokens
reduce decls@((Dk TEQ):(Dc comp):rest)      tokens = shift decls tokens
reduce decls@((Dk TGT):(Dc comp):rest)      tokens = shift decls tokens
reduce ((Da arth):(Dk TLTE):(Dc comp):rest) tokens = reduce ((Dc (Lte comp arth)):rest)  tokens
reduce ((Da arth):(Dk TGTE):(Dc comp):rest) tokens = reduce ((Dc (Gte comp arth)):rest)  tokens
reduce ((Da arth):(Dk TNE):(Dc comp):rest)  tokens = reduce ((Dc (Ne  comp arth)):rest)  tokens
reduce ((Da arth):(Dk TLT):(Dc comp):rest)  tokens = reduce ((Dc (Lt  comp arth)):rest)  tokens
reduce ((Da arth):(Dk TEQ):(Dc comp):rest)  tokens = reduce ((Dc (Eq  comp arth)):rest)  tokens
reduce ((Da arth):(Dk TGT):(Dc comp):rest)  tokens = reduce ((Dc (Gt  comp arth)):rest)  tokens

{- and operation -}
reduce decls@((Dd andop):rest)      tokens@(AND:_) = shift decls tokens
reduce decls@((Dk AND):(Dd andop):rest)     tokens = shift decls tokens
reduce ((Dc comp):(Dk AND):(Dd andop):rest) tokens = reduce ((Dd (And andop comp)):rest)  tokens

{- or operation -}
reduce decls@((Do orop):rest)       tokens@(OR:_) = shift decls tokens
reduce decls@((Dk OR):(Do orop):rest)      tokens = shift decls tokens
reduce ((Dd andop):(Dk OR):(Do orop):rest) tokens = reduce ((Do (Or orop andop)):rest)  tokens

{- parentheses -}
reduce decls@((Dk LPAREN):rest)                 tokens = shift decls tokens
reduce decls@((Do orop):(Dk LPAREN):rest)       tokens = shift decls tokens
reduce ((Dk RPAREN):(Do orop):(Dk LPAREN):rest) tokens = reduce ((Dn (Np (Po orop))):rest) tokens

{- assign -}  -- assignments not allowed across lines
reduce decls@((Dn (Nd id)):rest)       tokens@(ASSN:_) = shift decls tokens
reduce decls@((Dk ASSN):(Dn (Nd id)):rs)  tokens@(_:_) = shift decls tokens
reduce ((Do orop):(Dk ASSN):(Dn (Nd name)):rs)  tokens = reduce ((Ds (Assn name orop)):rs) tokens

{- negation -}  -- negation not allowed across lines
reduce decls@((Dk MINUS):rest) tokens@(tok:_) = shift decls tokens
reduce ((Dn neg):(Dk MINUS):rest) tokens | negatable rest = reduce (Df (Fn neg):rest) tokens
    where negatable ((Da _):xs) = False
          negatable _           = True

{- promotions -}
reduce ((Dk TRUE):rest)      tokens = reduce ((Df (Fb True)):rest)  tokens
reduce ((Dk FALSE):rest)     tokens = reduce ((Df (Fb False)):rest) tokens
reduce ((Dk (INT int)):rest) tokens = reduce ((Dn (Ni int)):rest)   tokens
reduce ((Dk (ID id)):rest)   tokens = reduce ((Dn (Nd id)):rest)    tokens
reduce ((Dn neg):rest)       tokens = reduce ((Df (Fp neg)):rest)   tokens
reduce ((Df fact):rest)      tokens = reduce ((Dt (Tf fact)):rest)  tokens
reduce ((Dt term):rest)      tokens = reduce ((Da (At term)):rest)  tokens
reduce ((Da arth):rest)      tokens = reduce ((Dc (Ca arth)):rest)  tokens
reduce ((Dc comp):rest)      tokens = reduce ((Dd (Ac comp)):rest)  tokens
reduce ((Dd andop):rest)     tokens = reduce ((Do (Oa andop)):rest) tokens
reduce ((Do orop):rest)      tokens = reduce ((Ds (So orop)):rest)  tokens

{- base cases -}
reduce decls@[_]    []      = return decls
reduce decls        tokens  = parseFail $ (show decls) ++ " " ++ (show tokens)

-- testing
parseStr :: String -> Either Failure [Decl]
parseStr str = extract (fst (tokenize str) >>= parse [])

parseTest = (p "1 + 2"  == Right [Ds (So (Oa (Ac (Ca (Add (At (Tf (Fp (Ni 1)))) (Tf (Fp (Ni 2))))))))])
         && (p "1 * 2"  == Right [Ds (So (Oa (Ac (Ca (At (Mult (Tf (Fp (Ni 1))) (Fp (Ni 2))))))))])
         && (p "1 + - 2 --2" == Right [Ds (So (Oa (Ac (Ca (Sub (Add (At (Tf (Fp (Ni 1)))) (Tf (Fn (Ni 2)))) (Tf (Fn (Ni 2))))))))])
         && (p "1 + 2 * 3 * -3 - -3 + 4 * 5 / 6" ==
            Right [Ds (So (Oa (Ac (Ca (Add (Sub (Add (At (Tf (Fp (Ni 1)))) (Mult (Mult (Tf (Fp (Ni 2))) (Fp (Ni 3))) (Fn (Ni 3)))) (Tf (Fn (Ni 3)))) (Div (Mult (Tf (Fp (Ni 4))) (Fp (Ni 5))) (Fp (Ni 6))))))))])
         && (p "1 - 2"    == Right [Ds (So (Oa (Ac (Ca (Sub (At (Tf (Fp (Ni 1)))) (Tf (Fp (Ni 2))))))))])
         && (p "- 1"      == Right [Ds (So (Oa (Ac (Ca (At (Tf (Fn (Ni 1))))))))])
         && (p "-(1 * 1)" == Right [Ds (So (Oa (Ac (Ca (At (Tf (Fn (Np (Po (Oa (Ac (Ca (At (Mult (Tf (Fp (Ni 1))) (Fp (Ni 1))))))))))))))))])
         && (p "-(-(-(-1)))" == Right [Ds (So (Oa (Ac (Ca (At (Tf (Fn (Np (Po (Oa (Ac (Ca (At (Tf (Fn (Np (Po (Oa (Ac (Ca (At (Tf (Fn (Np (Po (Oa (Ac (Ca (At (Tf (Fn (Ni 1))))))))))))))))))))))))))))))))])
         && (p "-1 * -(2 + 3 * 4)" == Right [Ds (So (Oa (Ac (Ca (At (Mult (Tf (Fn (Ni 1))) (Fn (Np (Po (Oa (Ac (Ca (Add (At (Tf (Fp (Ni 2)))) (Mult (Tf (Fp (Ni 3))) (Fp (Ni 4))))))))))))))))])
         && (p "abc: 1 - 3" == Right [Ds (Assn "abc" (Oa (Ac (Ca (Sub (At (Tf (Fp (Ni 1)))) (Tf (Fp (Ni 3))))))))])
         && (p "abc: def" == Right [Ds (Assn "abc" (Oa (Ac (Ca (At (Tf (Fp (Nd "def"))))))))])
         && (p "a: (b - c / 2) * d" == Right [Ds (Assn "a" (Oa (Ac (Ca (At (Mult (Tf (Fp (Np (Po (Oa (Ac (Ca (Sub (At (Tf (Fp (Nd "b")))) (Div (Tf (Fp (Nd "c"))) (Fp (Ni 2))))))))))) (Fp (Nd "d"))))))))])
         && (p "a\n:" == Right [Ds (So (Oa (Ac (Ca (At (Tf (Fp (Nd "a"))))))))]) -- not assign
         && (p "a or b and (c or d)" == Right [Ds (So (Or (Oa (Ac (Ca (At (Tf (Fp (Nd "a"))))))) (And (Ac (Ca (At (Tf (Fp (Nd "b")))))) (Ca (At (Tf (Fp (Np (Po (Or (Oa (Ac (Ca (At (Tf (Fp (Nd "c"))))))) (Ac (Ca (At (Tf (Fp (Nd "d"))))))))))))))))])
         && (p "a = b > c * d <= e or f" == Right [Ds (So (Or (Oa (Ac (Lte (Gt (Eq (Ca (At (Tf (Fp (Nd "a"))))) (At (Tf (Fp (Nd "b"))))) (At (Mult (Tf (Fp (Nd "c"))) (Fp (Nd "d"))))) (At (Tf (Fp (Nd "e"))))))) (Ac (Ca (At (Tf (Fp (Nd "f"))))))))])
         && (p "a = b > c * d <= e or f" == Right [Ds (So (Or (Oa (Ac (Lte (Gt (Eq (Ca (At (Tf (Fp (Nd "a"))))) (At (Tf (Fp (Nd "b"))))) (At (Mult (Tf (Fp (Nd "c"))) (Fp (Nd "d"))))) (At (Tf (Fp (Nd "e"))))))) (Ac (Ca (At (Tf (Fp (Nd "f"))))))))])
         && (p "(a >= b or c < d) != (3 and true)" ==
             Right [Ds (So (Oa (Ac (Ne (Ca (At (Tf (Fp (Np (Po (Or (Oa (Ac (Gte (Ca (At (Tf (Fp (Nd "a"))))) (At (Tf (Fp (Nd "b"))))))) (Ac (Lt (Ca (At (Tf (Fp (Nd "c"))))) (At (Tf (Fp (Nd "d"))))))))))))) (At (Tf (Fp (Np (Po (Oa (And (Ac (Ca (At (Tf (Fp (Ni 3)))))) (Ca (At (Tf (Fb True)))))))))))))))])
         
         && (p "1 + - - \n 1" == Left "<parse> [Dk MINUS,Dk MINUS,Dk PLUS,Da (At (Tf (Fp (Ni 1))))] []")
         && (p "-- 1" == Left "<parse> [Ds (So (Oa (Ac (Ca (At (Tf (Fn (Ni 1)))))))),Dk MINUS] []")
         && (p "a:" == Left "<parse> [Dk ASSN,Dn (Nd \"a\")] []")
         && (p "a+b: 3" == Left "<parse> [Ds (Assn \"b\" (Oa (Ac (Ca (At (Tf (Fp (Ni 3)))))))),Dk PLUS,Da (At (Tf (Fp (Nd \"a\"))))] []")
         && (p "-true" == Left "<parse> [Ds (So (Oa (Ac (Ca (At (Tf (Fb True))))))),Dk MINUS] []")
         where  p = parseStr
                
