module Tokenizer (
    Token(..),
    tokenize,
) where

import qualified Data.Map as Map
import Control.Monad (liftM, liftM2)
import Data.Char (isDigit, isOctDigit, isHexDigit, digitToInt, chr)

import Failure

data Token = INT Integer
           | ID String
           | STR String
           | TRUE | FALSE
           | NOTHING
           | DOT
           | LBRA | RBRA
           | AT
           | PLUS | MINUS | MULT | DIV
           | CONCAT
           | TLT | TEQ | TGT
           | TLTE | TGTE | TNE
           | AND | OR
           | COLON
           | IF | THEN | ELSE | OTHER | FI
           | WHILE | REPEAT
           | LPAREN | RPAREN
           | COMMA
           deriving (Show, Eq)

isNewline x = x `elem` "\n\r"
isSpace x   = x `elem` " \f\t\v"
isLetter x  = x `elem` (['a'..'z'] ++ ['A'..'Z'])
isStr x     = (x == '\'')

keywords :: Map.Map String Token
keywords = Map.fromList [
        ("True",      TRUE),
        ("False",     FALSE),
        ("Nothing",   NOTHING),
        ("and",       AND),
        ("or",        OR),
        ("if",        IF),
        ("then",      THEN),
        ("else",      ELSE),
        ("otherwise", OTHER),
        ("fi",        FI),
        ("while",     WHILE),
        ("repeat",    REPEAT)
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
getOneToken ('.':xs)        = (return DOT,    xs)
getOneToken ('[':xs)        = (return LBRA,   xs)
getOneToken (']':xs)        = (return RBRA,   xs)
getOneToken ('+':xs)        = (return PLUS,   xs)
getOneToken ('-':xs)        = (return MINUS,  xs)
getOneToken ('*':xs)        = (return MULT,   xs)
getOneToken ('/':xs)        = (return DIV,    xs)
getOneToken ('(':xs)        = (return LPAREN, xs)
getOneToken (')':xs)        = (return RPAREN, xs)
getOneToken (':':xs)        = (return COLON,  xs)
getOneToken ('&':xs)        = (return CONCAT, xs)
getOneToken ('<':'=':xs)    = (return TLTE,   xs)
getOneToken ('>':'=':xs)    = (return TGTE,   xs)
getOneToken ('!':'=':xs)    = (return TNE,    xs)
getOneToken ('<':xs)        = (return TLT,    xs)
getOneToken ('=':xs)        = (return TEQ,    xs)
getOneToken ('>':xs)        = (return TGT,    xs)
getOneToken (',':xs)        = (return COMMA,  xs)

getOneToken str@(x:xs)
    | isStr x        = getStr xs
    | isDigit x      = getNum str
    | isLetter x     = getIdOrKeyword $ str
    | otherwise      = (tokenizeFail $ "umatched token " ++ (show x), xs)
    where getIdOrKeyword = filterKeyword . getId

getStr :: String -> (Failable Token, String)
getStr ('\'':rest)       = (return (STR ""), rest)
getStr ('\\':'a':rest)   = prepend '\a' (getStr rest)
getStr ('\\':'b':rest)   = prepend '\b' (getStr rest)
getStr ('\\':'f':rest)   = prepend '\f' (getStr rest)
getStr ('\\':'n':rest)   = prepend '\n' (getStr rest)
getStr ('\\':'r':rest)   = prepend '\r' (getStr rest)
getStr ('\\':'t':rest)   = prepend '\t' (getStr rest)
getStr ('\\':'v':rest)   = prepend '\v' (getStr rest)
getStr ('\\':'\'':rest)  = prepend '\'' (getStr rest)
getStr ('\\':'\"':rest)  = prepend '\"' (getStr rest)
getStr ('\\':'\\':rest)  = prepend '\\' (getStr rest)
getStr ('\\':a:b:c:rest)
    | all isOctDigit [a,b,c] = prepend (charOf [a,b,c]) (getStr rest)
        where charOf = chr . parseInt 8
getStr ('\\':'x':a:b:rest)
    | all isHexDigit [a,b] = prepend (charOf [a,b]) (getStr rest)
         where charOf = chr . parseInt 16
getStr ('\\':'u':a:b:c:d:rest)
    | all isHexDigit [a,b,c,d] = prepend (charOf [a,b,c,d]) (getStr rest)
         where charOf = chr . parseInt 16
getStr ('\\':'0':rest)   = prepend '\0' (getStr rest)
getStr (x:rest)          = prepend x    (getStr rest)
        where (restStr, untokenized) = getStr rest

prepend :: Char -> (Failable Token, String) -> (Failable Token, String)
prepend char (token, rest) = (newToken, rest)
        where prependToToken (STR s) = STR (char:s)
              newToken = prependToToken `liftM` token

getNum :: String -> (Failable Token, String)
getNum str = (return (INT (read numStr)), rest)
             where (numStr, rest) = span isDigit str

getId :: String -> (Failable Token, String)
getId (x:xs) = (return (ID (x:idTail)), rest)
               where (idTail, rest) = span isIdChar xs
                     isIdChar x = (isDigit x) || (isLetter x)

filterKeyword :: (Failable Token, String) -> (Failable Token, String)
filterKeyword (token, input) = (newToken, input)
    where newToken = getNew `liftM` token
          getNew def@(ID name) = Map.findWithDefault def name keywords

          
parseInt :: Int -> String -> Int 
parseInt radix = foldl (\acc x -> acc * radix + digitToInt x) 0