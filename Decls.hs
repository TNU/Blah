module Decls (
    Line(..),
    Stmt(..),
    IfStmt(..),
    WhileStmt(..),
    AssnStmt(..),
    ToStmt(..),
    Params(..),
    ReturnStmt(..),
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
) where

import Tokens

-- decls are showable and equable for testing
data Decl = Dl Line
          | Ds Stmt
          | Dm Params
          | Db OrOp
          | Dd AndOp
          | Dz NotOp
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
          | St ToStmt
          | Sr ReturnStmt
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

data ToStmt = Func String Params Line
              deriving (Show, Eq)

data Params = Pempty
            | Pcons Params String
            deriving (Show, Eq)

data ReturnStmt = Return OrOp
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
