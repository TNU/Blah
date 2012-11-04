module Tokenizer (
    Token(..),
    tokenize,
) where

import qualified Data.Map as Map

import Control.Monad (liftM, liftM2)
import Data.Char (isDigit, isAlpha, isOctDigit, isHexDigit, digitToInt, chr)

import State

data Token = INT Integer
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
           | AND | OR
           | COLON
           | IF | THEN | ELSE | OTHER | FI
           | WHILE | REPEAT
           | LPAREN | RPAREN
           | COMMA
           deriving (Show, Eq)

isNewline x = x `elem` "\n\r"
isSpace x   = x `elem` " \f\t\v"
isId x      = isAlpha x || x == '_'

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

tokenize :: Runtime [Token]
tokenize = do
    isEof <- isEOF
    if isEof
    then return []
    else do line <- readLine
            tokenizeLineStr line

tokenizeLineStr :: String -> Runtime [Token]
tokenizeLineStr [] = return []
tokenizeLineStr (x:xs)
    | isNewline x   = return []
    | isSpace x     = tokenizeLineStr xs
tokenizeLineStr str = getOneToken str

addToken :: Token -> String -> Runtime [Token]
addToken token rest = (token:) `liftM` tokenizeLineStr rest

getOneToken :: String -> Runtime [Token]
getOneToken ('[':xs)     = addToken LBRA    xs
getOneToken (']':xs)     = addToken RBRA    xs
getOneToken ('+':xs)     = addToken PLUS    xs
getOneToken ('-':xs)     = addToken MINUS   xs
getOneToken ('*':xs)     = addToken MULT    xs
getOneToken ('/':xs)     = addToken DIV     xs
getOneToken ('(':xs)     = addToken LPAREN  xs
getOneToken (')':xs)     = addToken RPAREN  xs
getOneToken (':':xs)     = addToken COLON   xs
getOneToken ('&':xs)     = addToken CONCAT  xs
getOneToken ('<':'=':xs) = addToken TLTE    xs
getOneToken ('>':'=':xs) = addToken TGTE    xs
getOneToken ('!':'=':xs) = addToken TNE     xs
getOneToken ('<':xs)     = addToken TLT     xs
getOneToken ('=':xs)     = addToken TEQ     xs
getOneToken ('>':xs)     = addToken TGT     xs
getOneToken ('.':xs)     = addToken DOT     xs
getOneToken (',':xs)     = addToken COMMA   xs

getOneToken ('\'':xs)    = getStr xs

getOneToken str@(x:xs)
    | isDigit x     = getNum str
    | isId x        = getId str >>= filterKeyword

getOneToken (x:xs)  = tokenizeFail $ "umatched token " ++ (show x)

getNum :: String -> Runtime [Token]
getNum str = addToken (INT (read numStr)) rest
    where (numStr, rest) = span isDigit str

getId :: String -> Runtime [Token]
getId (x:xs) = addToken (ID (x:idTail)) rest
    where (idTail, rest) = span isRestId xs
          isRestId x = isId x || isDigit x

filterKeyword :: [Token] -> Runtime [Token]
filterKeyword ((def@(ID name)):rest) = return (newToken:rest)
    where newToken = Map.findWithDefault def name keywords

getStr :: String -> Runtime [Token]
getStr ('\'':rest)       = addToken (STR "") rest
getStr ('\\':'a':rest)   = addChar '\a' rest
getStr ('\\':'b':rest)   = addChar '\b' rest
getStr ('\\':'f':rest)   = addChar '\f' rest
getStr ('\\':'n':rest)   = addChar '\n' rest
getStr ('\\':'r':rest)   = addChar '\r' rest
getStr ('\\':'t':rest)   = addChar '\t' rest
getStr ('\\':'v':rest)   = addChar '\v' rest
getStr ('\\':'\'':rest)  = addChar '\'' rest
getStr ('\\':'\"':rest)  = addChar '\"' rest
getStr ('\\':'\\':rest)  = addChar '\\' rest
getStr ('\\':a:b:c:rest)
    | all isOctDigit [a,b,c] = addChar (chr (parseInt 8 [a,b,c])) rest
getStr ('\\':'x':a:b:rest)
    | all isHexDigit [a,b] = addChar (chr (parseInt 16 [a,b])) rest
getStr ('\\':'u':a:b:c:d:rest)
    | all isHexDigit [a,b,c,d] = addChar (chr (parseInt 16 [a,b,c,d])) rest
getStr ('\\':'0':rest)   = addChar '\0' rest
getStr (x:rest)          = addChar x    rest
getStr [] = do isEof <- isEOF
               if isEof then tokenizeFail $ "unterminated string"
                        else readLine >>= addChar '\n'

parseInt :: Int -> String -> Int
parseInt radix = foldl (\acc x -> acc * radix + digitToInt x) 0

addChar :: Char -> String -> Runtime [Token]
addChar char rest = getStr rest >>= prependWith char
    where prependWith c ((STR str):ts) = return ((STR (c:str)):ts)
