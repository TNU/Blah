module Parser (
    Line(..),
    Stmt(..),
    IfStmt(..),
    WhileStmt(..),
    AssnStmt(..),
    Decl(..),
    OrOp(..),
    AndOp(..),
    NotOp(..),
    Comp(..),
    Join(..),
    Arth(..),
    Term(..),
    Factor(..),
    List(..),
    Neg(..),
    Obj(..),
    Elem(..),
    Args(..),
    Call(..),
    Paren(..),
    parse,
) where

import Data.Functor.Identity
import Control.Monad.Trans.Error

import Failure (parseFail)
import Tokenizer (Token(..))
import State (Runtime)

-- decls are showable and equable for testing
data Decl = Dl Line
          | Ds Stmt
          | Db OrOp
          | Dd AndOp
          | Dm NotOp
          | Dc Comp
          | Dj Join
          | Da Arth
          | Dt Term
          | Df Factor
          | Di List
          | Dk Token
          | Dn Neg
          | Do Obj
          | De Elem
          | Dp Paren
          | Dr Args
          | Dh Call
          | Dnewline
          deriving (Show, Eq)

data Line = Ls Stmt
          | Lm Line Stmt
          deriving (Show, Eq)

data Stmt = Se OrOp
          | Si IfStmt
          | Sw WhileStmt
          | Sa AssnStmt
          deriving (Show, Eq)

data IfStmt = If OrOp Line
            | IfOther OrOp Line Line
            | IfElse OrOp Line IfStmt
            deriving (Show, Eq)

data WhileStmt = While OrOp Line
               deriving (Show, Eq)

data AssnStmt = AssnId String OrOp
              | AssnElem Elem OrOp
              deriving (Show, Eq)

data OrOp = Oa AndOp
          | Or OrOp AndOp
          deriving (Show, Eq)

data AndOp = An NotOp
           | And AndOp NotOp
           deriving (Show, Eq)

data NotOp = Mc Comp
           | Not Comp
           deriving (Show, Eq)

data Comp = Cj  Join
          | Lt  Comp Join
          | Eq  Comp Join
          | Gt  Comp Join
          | Lte Comp Join
          | Gte Comp Join
          | Neq Comp Join
          deriving (Show, Eq)

data Join = Ja Arth
          | Concat Join Arth
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
            | Fs String
            | Fl List
            | Fnothing
            deriving (Show, Eq)

data List = Lempty
          | Lone OrOp
          | Lcons List OrOp
          deriving (Show, Eq)

data Neg  = Ni Int
          | Nd String
          | Np Paren
          | No Obj
          | Ne Elem
          | Nc Call
          deriving (Show, Eq)

data Obj = PropS String String
         | PropD String String
         | PropL List   String
         | PropP Paren  String
         | PropO Obj    String
         | PropE Elem   String
         deriving (Show, Eq)

data Elem = ElemS String OrOp
          | ElemD String OrOp
          | ElemL List   OrOp
          | ElemP Paren  OrOp
          | ElemO Obj    OrOp
          | ElemE Elem   OrOp
          | ElemC Call   OrOp
          deriving (Show, Eq)

data Args = Rempty
          | Rcons Args OrOp
          deriving (Show, Eq)

data Call = Call Neg Args
          deriving (Show, Eq)

data Paren = Pe OrOp
           deriving (Show, Eq)

parse :: [Decl] -> [Token] -> Runtime [Decl]
parse []          = shift []
parse (last:rest) = reduce ((demote last):rest)

shift :: [Decl] -> [Token] -> Runtime [Decl]
shift decls (tok:tokens) = reduce ((Dk tok):decls) tokens
shift decls []           = return decls

reduce :: [Decl] -> [Token] -> Runtime [Decl]

{- object -}
reduce decls@((Dk (STR s)):rest)                 tokens@(DOT:_) = shift decls tokens
reduce decls@((Dk (ID d)):rest)                  tokens@(DOT:_) = shift decls tokens
reduce decls@((Df (Fl l)):rest)                  tokens@(DOT:_) = shift decls tokens
reduce decls@((Dp paren):rest)                   tokens@(DOT:_) = shift decls tokens
reduce decls@((Do obj):rest)                     tokens@(DOT:_) = shift decls tokens
reduce decls@((De elem):rest)                    tokens@(DOT:_) = shift decls tokens
reduce decls@((Dk DOT):(Dk (STR x)):rest)                tokens = shift decls tokens
reduce decls@((Dk DOT):(Dk (ID x)):rest)                 tokens = shift decls tokens
reduce decls@((Dk DOT):(Df (Fl x)):rest)                      tokens = shift decls tokens
reduce decls@((Dk DOT):(Dp x):rest)                      tokens = shift decls tokens
reduce decls@((Dk DOT):(Do x):rest)                      tokens = shift decls tokens
reduce decls@((Dk DOT):(De x):rest)                      tokens = shift decls tokens
reduce decls@((Dk (ID prop)):(Dk DOT):(Dk (STR x)):rest) tokens = reduce ((Do (PropS x prop)):rest) tokens
reduce decls@((Dk (ID prop)):(Dk DOT):(Dk (ID x)):rest)  tokens = reduce ((Do (PropD x prop)):rest) tokens
reduce decls@((Dk (ID prop)):(Dk DOT):(Df (Fl x)):rest)  tokens = reduce ((Do (PropL x prop)):rest) tokens
reduce decls@((Dk (ID prop)):(Dk DOT):(Dp x):rest)       tokens = reduce ((Do (PropP x prop)):rest) tokens
reduce decls@((Dk (ID prop)):(Dk DOT):(Do x):rest)       tokens = reduce ((Do (PropO x prop)):rest) tokens
reduce decls@((Dk (ID prop)):(Dk DOT):(De x):rest)       tokens = reduce ((Do (PropE x prop)):rest) tokens

{- elem -}
reduce decls@((Dk (STR s)):rest)             tokens@(LBRA:_) = shift decls tokens
reduce decls@((Dk (ID d)):rest)              tokens@(LBRA:_) = shift decls tokens
reduce decls@((Df (Fl l)):rest)              tokens@(LBRA:_) = shift decls tokens
reduce decls@((Dp paren):rest)               tokens@(LBRA:_) = shift decls tokens
reduce decls@((Do obj):rest)                 tokens@(LBRA:_) = shift decls tokens
reduce decls@((De elem):rest)                tokens@(LBRA:_) = shift decls tokens
reduce decls@((Dc call):rest)                tokens@(LBRA:_) = shift decls tokens
reduce decls@((Dk LBRA):(Dk (STR s)):rest)            tokens = shift decls tokens
reduce decls@((Dk LBRA):(Dk (ID d)):rest)             tokens = shift decls tokens
reduce decls@((Dk LBRA):(Df (Fl l)):rest)             tokens = shift decls tokens
reduce decls@((Dk LBRA):(Dp paren):rest)              tokens = shift decls tokens
reduce decls@((Dk LBRA):(Do obj):rest)                tokens = shift decls tokens
reduce decls@((Dk LBRA):(De elem):rest)               tokens = shift decls tokens
reduce decls@((Dk LBRA):(Dh call):rest)               tokens = shift decls tokens
reduce decls@((Db i):(Dk LBRA):(Dk (STR x)):rest)     tokens = shift decls tokens
reduce decls@((Db i):(Dk LBRA):(Dk (ID x)):rest)      tokens = shift decls tokens
reduce decls@((Db i):(Dk LBRA):(Df (Fl x)):rest)      tokens = shift decls tokens
reduce decls@((Db i):(Dk LBRA):(Dp x):rest)           tokens = shift decls tokens
reduce decls@((Db i):(Dk LBRA):(Do x):rest)           tokens = shift decls tokens
reduce decls@((Db i):(Dk LBRA):(De x):rest)           tokens = shift decls tokens
reduce decls@((Db i):(Dk LBRA):(Dh x):rest)           tokens = shift decls tokens
reduce ((Dk RBRA):(Db i):(Dk LBRA):(Dk (STR x)):rest) tokens = reduce ((De (ElemS x i)):rest) tokens
reduce ((Dk RBRA):(Db i):(Dk LBRA):(Dk (ID x)):rest)  tokens = reduce ((De (ElemD x i)):rest) tokens
reduce ((Dk RBRA):(Db i):(Dk LBRA):(Df (Fl x)):rest)  tokens = reduce ((De (ElemL x i)):rest) tokens
reduce ((Dk RBRA):(Db i):(Dk LBRA):(Dp x):rest)       tokens = reduce ((De (ElemP x i)):rest) tokens
reduce ((Dk RBRA):(Db i):(Dk LBRA):(Do x):rest)       tokens = reduce ((De (ElemO x i)):rest) tokens
reduce ((Dk RBRA):(Db i):(Dk LBRA):(De x):rest)       tokens = reduce ((De (ElemE x i)):rest) tokens
reduce ((Dk RBRA):(Db i):(Dk LBRA):(Dh x):rest)       tokens = reduce ((De (ElemC x i)):rest) tokens

{- list -}
reduce decls@((Dk LBRA):rest)                         tokens = shift decls tokens
reduce ((Dk RBRA):(Dk LBRA):rest)                     tokens = reduce ((Df (Fl Lempty)):rest) tokens
reduce ((Db orop):(Dk LBRA):rest)                     tokens = reduce ((Di (Lone orop)):(Dk LBRA):rest) tokens
reduce decls@((Di list):(Dk LBRA):rest)               tokens = shift decls tokens
reduce decls@((Dk COMMA):(Di list):(Dk LBRA):rest)    tokens = shift decls tokens
reduce ((Db x):(Dk COMMA):(Di list):(Dk LBRA):rest)   tokens = reduce ((Di (Lcons list x)):(Dk LBRA):rest) tokens
reduce ((Dk RBRA):(Di list):(Dk LBRA):rest)           tokens = reduce ((Df (Fl list)):rest) tokens

{- call -}
reduce decls@((Dn neg):rest)                        tokens@(LPAREN:_) = shift decls tokens
reduce decls@((Dk LPAREN):(Dn neg):rest)            tokens@(RPAREN:_) = shift decls tokens
reduce decls@((Dk LPAREN):(Dn neg):rest)                       tokens = reduce ((Dr Rempty):(Dk LPAREN):(Dn neg):rest) tokens
reduce decls@((Dr args):(Dk LPAREN):(Dn neg):rest)             tokens = shift decls tokens
reduce decls@((Db orop):(Dr args):(Dk LPAREN):(Dn neg):rest)   tokens = shift decls tokens
reduce decls@((Dk COMMA):(Db orop):(Dr args):(Dk LPAREN):(Dn neg):rest)  tokens = reduce ((Dr (Rcons args orop)):(Dk LPAREN):(Dn neg):rest) tokens
reduce decls@((Dk RPAREN):(Dk LPAREN):(Dn neg):rest)                     tokens = reduce ((Dh (Call neg Rempty)):rest) tokens
reduce decls@((Dk RPAREN):(Db orop):(Dr args):(Dk LPAREN):(Dn neg):rest) tokens = reduce ((Dh (Call neg (Rcons args orop))):rest) tokens

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
reduce ((Dj join):(Dk TLTE):(Dc comp):rest) tokens = reduce ((Dc (Lte comp join)):rest)  tokens
reduce ((Dj join):(Dk TGTE):(Dc comp):rest) tokens = reduce ((Dc (Gte comp join)):rest)  tokens
reduce ((Dj join):(Dk TNE):(Dc comp):rest)  tokens = reduce ((Dc (Neq  comp join)):rest)  tokens
reduce ((Dj join):(Dk TLT):(Dc comp):rest)  tokens = reduce ((Dc (Lt  comp join)):rest)  tokens
reduce ((Dj join):(Dk TEQ):(Dc comp):rest)  tokens = reduce ((Dc (Eq  comp join)):rest)  tokens
reduce ((Dj join):(Dk TGT):(Dc comp):rest)  tokens = reduce ((Dc (Gt  comp join)):rest)  tokens

{- not operation -}  -- not operation not allowed across lines
reduce decls@((Dk NOT):rest) tokens@(_:_) = shift decls tokens
reduce ((Dc comp):(Dk NOT):rest)   tokens = reduce ((Dm (Not comp)):rest) tokens

{- and operation -}
reduce decls@((Dd andop):rest)       tokens@(AND:_) = shift decls tokens
reduce decls@((Dk AND):(Dd andop):rest)      tokens = shift decls tokens
reduce ((Dm notop):(Dk AND):(Dd andop):rest) tokens = reduce ((Dd (And andop notop)):rest)  tokens

{- or operation -}
reduce decls@((Db orop):rest)       tokens@(OR:_) = shift decls tokens
reduce decls@((Dk OR):(Db orop):rest)      tokens = shift decls tokens
reduce ((Dd andop):(Dk OR):(Db orop):rest) tokens = reduce ((Db (Or orop andop)):rest)  tokens

{- parentheses -}
reduce decls@((Dk LPAREN):rest)                 tokens = shift decls tokens
reduce decls@((Db orop):(Dk LPAREN):rest)       tokens = shift decls tokens
reduce ((Dk RPAREN):(Db orop):(Dk LPAREN):rest) tokens = reduce ((Dp (Pe orop)):rest) tokens

{- assign -}  -- assignments not allowed across lines
reduce decls@((Dn (Nd id)):rest)      tokens@(COLON:_) = shift decls tokens
reduce decls@((De elem):rest)         tokens@(COLON:_) = shift decls tokens
reduce decls@((Dk COLON):(Dn (Nd id)):rs) tokens@(_:_) = shift decls tokens
reduce decls@((Dk COLON):(De elem):rs)    tokens@(_:_) = shift decls tokens
reduce ((Db orop):(Dk COLON):(Dn (Nd name)):rs) tokens = reduce ((Ds (Sa (AssnId name orop))):rs) tokens
reduce ((Db orop):(Dk COLON):(De elem):rs)      tokens = reduce ((Ds (Sa (AssnElem elem orop))):rs) tokens

{- join -}
reduce decls@((Dj join):rest)          tokens@(CONCAT:_) = shift decls tokens
reduce decls@((Dk CONCAT):(Dj join):rest)         tokens = shift decls tokens
reduce decls@((Da arth):(Dk CONCAT):(Dj join):rs) tokens = reduce ((Dj (Concat join arth)):rs) tokens

{- if -}
reduce decls@((Dk IF):rest)                      tokens = shift decls tokens
reduce decls@((Db orop):(Dk IF):rest)            tokens = shift decls tokens
reduce decls@((Dk THEN):(Db orop):(Dk IF):rest)  tokens = shift decls tokens
reduce decls@((Dl lines):(Dk THEN):(Db orop):(Dk IF):rest)      tokens = shift decls tokens
reduce ((Dk FI):(Dl lines):(Dk THEN):(Db orop):(Dk IF):rest)    tokens = reduce ((Ds (Si (If orop lines))):rest) tokens
reduce decls@((Dk OTHER):(Dl lines):(Dk THEN):(Db orop):(Dk IF):rest)    tokens = shift decls tokens
reduce decls@((Dl b):(Dk OTHER):(Dl a):(Dk THEN):(Db orop):(Dk IF):rest) tokens = shift decls tokens
reduce ((Dk FI):(Dl b):(Dk OTHER):(Dl a):(Dk THEN):(Db t):(Dk IF):rest)  tokens = reduce (Ds (Si (IfOther t a b)):rest) tokens
reduce decls@((Dk ELSE):(Dl a):(Dk THEN):(Db orop):(Dk IF):rest)         tokens = shift decls tokens
reduce ((Ds (Si ifStmt)):(Dk ELSE):(Dl a):(Dk THEN):(Db t):(Dk IF):rest) tokens = reduce (Ds (Si (IfElse t a ifStmt)):rest) tokens

reduce ((Dk OTHER):Dnewline:rest) tokens = reduce ((Dk OTHER):rest) tokens
reduce ((Dk ELSE):Dnewline:rest)  tokens = reduce ((Dk ELSE):rest) tokens
reduce ((Dk FI):Dnewline:rest)    tokens = reduce ((Dk FI):rest) tokens

{- while -}
reduce decls@((Dk WHILE):rest)                     tokens = shift decls tokens
reduce decls@((Db orop):(Dk WHILE):rest)           tokens = shift decls tokens
reduce decls@((Dk THEN):(Db orop):(Dk WHILE):rest) tokens = shift decls tokens
reduce decls@((Dl lines):(Dk THEN):(Db orop):(Dk WHILE):rest) tokens = shift decls tokens
reduce ((Dk REPEAT):(Dl lines):(Dk THEN):(Db orop):(Dk WHILE):rest) tokens = reduce (Ds (Sw (While orop lines)):rest) tokens

reduce ((Dk REPEAT):Dnewline:rest)           tokens = reduce ((Dk REPEAT):rest) tokens

{- comma line combination -}  -- comma cannot be the last character in a line
reduce decls@((Dl line):rest)          tokens@(COMMA:_) = shift decls tokens
reduce decls@((Dk COMMA):(Dl line):rest)   tokens@(_:_) = shift decls tokens
reduce decls@((Ds stmt):(Dk COMMA):(Dl line):rs) tokens = reduce ((Dl (Lm line stmt)):rs) tokens

{- automatic line combination -}
reduce decls@(Dnewline:(Dl line):rest)         tokens = shift decls tokens
reduce decls@((Ds stmt):Dnewline:(Dl line):rs) tokens = reduce ((Dl (Lm line stmt)):rs) tokens

{- negation -}  -- negation not allowed across lines
reduce decls@((Dk MINUS):rest)               tokens@(_:_) = shift decls tokens
reduce ((Dn neg):(Dk MINUS):rest) tokens | negatable rest = reduce ((Df (Fn neg)):rest) tokens
    where negatable ((Da _):xs) = False
          negatable _           = True

{- promotions -}
reduce ((Dk (ID id)):rest)   tokens = reduce ((Dn (Nd id)):rest)    tokens
reduce ((Dk (INT int)):rest) tokens = reduce ((Dn (Ni int)):rest)   tokens
reduce ((Dp paren):rest)     tokens = reduce ((Dn (Np paren)):rest) tokens
reduce ((De elem):rest)      tokens = reduce ((Dn (Ne elem)):rest)  tokens
reduce ((Do obj):rest)       tokens = reduce ((Dn (No obj)):rest)   tokens
reduce ((Dh call):rest)      tokens = reduce ((Dn (Nc call)):rest)  tokens
reduce ((Dn neg):rest)       tokens = reduce ((Df (Fp neg)):rest)   tokens
reduce ((Dk NOTHING):rest)   tokens = reduce ((Df (Fnothing)):rest) tokens
reduce ((Dk (STR s)):rest)   tokens = reduce ((Df (Fs s)):rest)     tokens
reduce ((Dk TRUE):rest)      tokens = reduce ((Df (Fb True)):rest)  tokens
reduce ((Dk FALSE):rest)     tokens = reduce ((Df (Fb False)):rest) tokens
reduce ((Df fact):rest)      tokens = reduce ((Dt (Tf fact)):rest)  tokens
reduce ((Dt term):rest)      tokens = reduce ((Da (At term)):rest)  tokens
reduce ((Da arth):rest)      tokens = reduce ((Dj (Ja arth)):rest)  tokens
reduce ((Dj join):rest)      tokens = reduce ((Dc (Cj join)):rest)  tokens
reduce ((Dc comp):rest)      tokens = reduce ((Dm (Mc comp)):rest)  tokens
reduce ((Dm notop):rest)     tokens = reduce ((Dd (An notop)):rest)  tokens
reduce ((Dd andop):rest)     tokens = reduce ((Db (Oa andop)):rest) tokens
reduce ((Db orop):rest)      tokens = reduce ((Ds (Se orop)):rest)  tokens
reduce ((Ds stmt):rest)      tokens = reduce ((Dl (Ls stmt)):rest)  tokens

{- base cases -}
reduce decls@[Dl _] []      = return decls
reduce decls        tokens  = parseFail $ (show decls) ++ " " ++ (show tokens)

{- demotions -}
demote :: Decl -> Decl
demote (Ds (Se orop))  = demote (Db orop)
demote (Db (Oa andop)) = demote (Dd andop)
demote (Dd (An notop)) = demote (Dm notop)
demote (Dm (Mc comp))  = demote (Dc comp)
demote (Dc (Cj join))  = demote (Dj join)
demote (Dj (Ja arth))  = demote (Da arth)
demote (Da (At term))  = demote (Dt term)
demote (Dt (Tf fact))  = demote (Df fact)
demote x               = x
