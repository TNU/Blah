module Parser (
    Line(..),
    Stmt(..),
    IfStmt(..),
    WhileStmt(..),
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

-- decls are showable and equable for testing
data Decl = Dl Line
          | Ds Stmt
          | Do OrOp
          | Dd AndOp
          | Dc Comp
          | Da Arth
          | Dt Term
          | Df Factor
          | Dk Token
          | Dn Neg
          | Dnewline
          deriving (Show, Eq)

data Line = Ls Stmt
          | Lm Line Stmt
          deriving (Show, Eq)

data Stmt = Se OrOp
          | Si IfStmt
          | Sw WhileStmt
          | Assn String OrOp
          deriving (Show, Eq)

data IfStmt = If OrOp Line
            | IfOther OrOp Line Line
            | IfElse OrOp Line IfStmt
            deriving (Show, Eq)

data WhileStmt = While OrOp Line
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
parse []    = shift []
parse decls = reduce (demoteLast decls)

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
reduce decls@((Dn (Nd id)):rest)      tokens@(COLON:_) = shift decls tokens
reduce decls@((Dk COLON):(Dn (Nd id)):rs) tokens@(_:_) = shift decls tokens
reduce ((Do orop):(Dk COLON):(Dn (Nd name)):rs) tokens = reduce ((Ds (Assn name orop)):rs) tokens

{- if -}
reduce decls@((Dk IF):rest)                      tokens = shift decls tokens
reduce decls@((Do orop):(Dk IF):rest)            tokens = shift decls tokens
reduce decls@((Dk THEN):(Do orop):(Dk IF):rest)  tokens = shift decls tokens
reduce decls@((Dl lines):(Dk THEN):(Do orop):(Dk IF):rest)               tokens = shift decls tokens
reduce decls@((Dk FI):(Dl lines):(Dk THEN):(Do orop):(Dk IF):rest)       tokens = reduce ((Ds (Si (If orop lines))):rest) tokens
reduce decls@((Dk OTHER):(Dl lines):(Dk THEN):(Do orop):(Dk IF):rest)    tokens = shift decls tokens
reduce decls@((Dl b):(Dk OTHER):(Dl a):(Dk THEN):(Do orop):(Dk IF):rest) tokens = shift decls tokens
reduce decls@((Dk FI):(Dl b):(Dk OTHER):(Dl a):(Dk THEN):(Do t):(Dk IF):rest) tokens = reduce (Ds (Si (IfOther t a b)):rest) tokens
reduce decls@((Dk ELSE):(Dl a):(Dk THEN):(Do orop):(Dk IF):rest)         tokens = shift decls tokens
reduce decls@((Ds (Si ifStmt)):(Dk ELSE):(Dl a):(Dk THEN):(Do t):(Dk IF):rest) tokens = reduce (Ds (Si (IfElse t a ifStmt)):rest) tokens

reduce decls@((Dk OTHER):Dnewline:rest) tokens = reduce ((Dk OTHER):rest) tokens
reduce decls@((Dk ELSE):Dnewline:rest)  tokens = reduce ((Dk ELSE):rest) tokens
reduce decls@((Dk FI):Dnewline:rest)    tokens = reduce ((Dk FI):rest) tokens

{- while -}
reduce decls@((Dk WHILE):rest)                     tokens = shift decls tokens
reduce decls@((Do orop):(Dk WHILE):rest)           tokens = shift decls tokens
reduce decls@((Dk THEN):(Do orop):(Dk WHILE):rest) tokens = shift decls tokens
reduce decls@((Dl lines):(Dk THEN):(Do orop):(Dk WHILE):rest) tokens = shift decls tokens
reduce decls@((Dk REPEAT):(Dl lines):(Dk THEN):(Do orop):(Dk WHILE):rest) tokens = reduce (Ds (Sw (While orop lines)):rest) tokens
reduce decls@((Dk REPEAT):Dnewline:rest)           tokens = reduce ((Dk REPEAT):rest) tokens

{- comma line combination -}  -- comma cannot be the last character in a line
reduce decls@((Dl line):rest)          tokens@(COMMA:_) = shift decls tokens
reduce decls@((Dk COMMA):(Dl line):rest)   tokens@(_:_) = shift decls tokens
reduce decls@((Ds stmt):(Dk COMMA):(Dl line):rs) tokens = reduce ((Dl (Lm line stmt)):rs) tokens

{- automatic line combination -}
reduce decls@(Dnewline:(Dl line):rest)         tokens = shift decls tokens
reduce decls@((Ds stmt):Dnewline:(Dl line):rs) tokens = reduce ((Dl (Lm line stmt)):rs) tokens

{- negation -}  -- negation not allowed across lines
reduce decls@((Dk MINUS):rest)               tokens@(_:_) = shift decls tokens
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
reduce ((Do orop):rest)      tokens = reduce ((Ds (Se orop)):rest)  tokens
reduce ((Ds stmt):rest)      tokens = reduce ((Dl (Ls stmt)):rest)  tokens

{- base cases -}
reduce decls@[Dl _] []      = return decls
reduce decls        tokens  = parseFail $ (show decls) ++ " " ++ (show tokens)

{- demotions -}
demoteLast :: [Decl] -> [Decl]
demoteLast (x:xs)  = (demote x):xs
demoteLast []      = []

demote :: Decl -> Decl
demote (Ds (Se orop))  = demote (Do orop)
demote (Do (Oa andop)) = demote (Dd andop)
demote (Dd (Ac comp))  = demote (Dc comp)
demote (Dc (Ca arth))  = demote (Da arth)
demote (Da (At term))  = demote (Dt term)
demote (Dt (Tf fact))  = demote (Df fact)
demote x               = x

-- testing
parseStr :: String -> Either Failure [Decl]
parseStr str = extract (fst (tokenize str) >>= parse [])
