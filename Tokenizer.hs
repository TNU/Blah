module Tokenizer (
    Token(..),
    tokenize,
) where

import Data.List (span)
import Control.Monad (liftM2)

import Failure

data Token = INT Integer
           | ID String
           | PLUS | MINUS | MULT | DIV
           | LPAREN | RPAREN
           | ASSN
           deriving (Show, Eq)

isNewline x = x `elem` "\n\r"
isSpace x   = x `elem` " \f\t\v"
isNum x     = x `elem` ['0'..'9']
isLetter x  = x `elem` (['a'..'z'] ++ ['A'..'Z'])

tokenize :: String -> (Failable [Token], String)
tokenize [] = (return [], "")
tokenize (x:xs)
    | isNewline x   = (return [], xs)
    | isSpace x     = tokenize xs
tokenize str = (curTokens, restLines)
        where (token, rest) = getOneToken str
              (restToks, restLines) = tokenize rest
              curTokens = liftM2 (:) token restToks

getOneToken :: String -> (Failable Token, String)
getOneToken ('+':xs) = (return PLUS,    xs)
getOneToken ('-':xs) = (return MINUS,   xs)
getOneToken ('*':xs) = (return MULT,    xs)
getOneToken ('/':xs) = (return DIV,     xs)
getOneToken ('(':xs) = (return LPAREN,  xs)
getOneToken (')':xs) = (return RPAREN,  xs)
getOneToken (':':xs) = (return ASSN,    xs)
getOneToken str@(x:xs)
    | isNum x        = getNum str
    | isLetter x     = getId str
    | otherwise      = (tokenizeFail $ "umatched token " ++ (show x), xs)

getNum :: String -> (Failable Token, String)
getNum str = (return (INT (read numStr)), rest)
             where (numStr, rest) = span isNum str

getId :: String -> (Failable Token, String)
getId (x:xs) = (return (ID (x:idTail)), rest)
               where (idTail, rest) = span isNumOrLetter xs
                     isNumOrLetter x = (isNum x) || (isLetter x)
