module Tokens (
    Token(..),
) where

data Token = INT Int
           | ID String
           | STR String
           | TRUE | FALSE
           | NOTHING
           | DOT
           | LBRA | RBRA
           | PLUS | MINUS | MULT | DIV
           | CONCAT
           | TLT | TEQ | TGT
           | TLTE | TGTE | TNE
           | AND | OR | NOT
           | COLON
           | IF | THEN | ELSE | OTHER | FI
           | WHILE | REPEAT
           | TO | WITH | DO | DONE | RETURN
           | LPAREN | RPAREN
           | COMMA
           deriving (Show, Eq)

