module Tokenizer (
    Token(..),
    tokenize,
) where

import qualified Data.Map as Map
import Control.Monad (liftM2)

import Failure

data Token = INT Integer
           | ID String
           | TRUE | FALSE

           | PLUS | MINUS | MULT | DIV
           
           | TLT | TEQ | TGT
           | TLTE | TGTE | TNE
           
           | AND | OR

           | ASSN
           | IF | ELSE | FI

           | LPAREN | RPAREN
           deriving (Show, Eq)

isNewline x = x `elem` "\n\r"
isSpace x   = x `elem` " \f\t\v"
isNum x     = x `elem` ['0'..'9']
isLetter x  = x `elem` (['a'..'z'] ++ ['A'..'Z'])

keywords :: Map.Map String Token
keywords = Map.fromList [
        ("true", TRUE), 
        ("false", FALSE),
        ("and", AND),   
        ("or", OR),
        ("if", IF),     
        ("else", ELSE),     
        ("fi", FI)
    ]

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
getOneToken ('+':xs)        = (return PLUS,    xs)
getOneToken ('-':xs)        = (return MINUS,   xs)
getOneToken ('*':xs)        = (return MULT,    xs)
getOneToken ('/':xs)        = (return DIV,     xs)
getOneToken ('(':xs)        = (return LPAREN,  xs)
getOneToken (')':xs)        = (return RPAREN,  xs)
getOneToken (':':xs)        = (return ASSN,    xs)
getOneToken ('<':'=':xs)    = (return TLTE,     xs)
getOneToken ('>':'=':xs)    = (return TGTE,     xs)
getOneToken ('!':'=':xs)    = (return TNE,     xs)
getOneToken ('<':xs)        = (return TLT,     xs)
getOneToken ('=':xs)        = (return TEQ,     xs)
getOneToken ('>':xs)        = (return TGT,     xs)

getOneToken str@(x:xs)
    | isNum x        = getNum str
    | isLetter x     = getIdOrKeyword $ str
    | otherwise      = (tokenizeFail $ "umatched token " ++ (show x), xs)
    where getIdOrKeyword = filterKeyword . getId    
        
getNum :: String -> (Failable Token, String)
getNum str = (return (INT (read numStr)), rest)
             where (numStr, rest) = span isNum str

getId :: String -> (Failable Token, String)
getId (x:xs) = (return (ID (x:idTail)), rest)
               where (idTail, rest) = span isNumOrLetter xs
                     isNumOrLetter x = (isNum x) || (isLetter x)

filterKeyword :: (Failable Token, String) -> (Failable Token, String)
filterKeyword (token, input) = (newToken, input)
    where newToken = token >>= return . getNew
          getNew def@(ID name) = Map.findWithDefault def name keywords