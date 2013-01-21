module Tokenizer (
    tokenize,
) where

import qualified Data.Map as Map

import Control.Monad (liftM)
import Data.Char (isDigit, isAlpha, isOctDigit, isHexDigit, digitToInt, chr)

import Failure (tokenizeFail)
import State
import Tokens

isId :: Char -> Bool
isId x      = isAlpha x || x == '_'

keywords :: Map.Map String Token
keywords = Map.fromList [
        ("True",      TRUE),
        ("False",     FALSE),
        ("Nothing",   NOTHING),
        ("and",       AND),
        ("or",        OR),
        ("not",       NOT),
        ("if",        IF),
        ("then",      THEN),
        ("else",      ELSE),
        ("otherwise", OTHER),
        ("fi",        FI),
        ("while",     WHILE),
        ("repeat",    REPEAT),
        ("to",        TO),
        ("with",      WITH),
        ("do",        DO),
        ("done",      DONE),
        ("return",    RETURN)
    ]

tokenize :: Runtime [Token]
tokenize = isEOF >>= ifIsEOF
    where ifIsEOF True  = return []
          ifIsEOF False = readLine >>= getNextToken

getNextToken :: String -> Runtime [Token]
getNextToken ('\n':_)     = return []
getNextToken ('\r':_)     = return []
getNextToken ('#':_)      = return []

getNextToken (' ':xs)     = getNextToken xs
getNextToken ('\f':xs)    = getNextToken xs
getNextToken ('\t':xs)    = getNextToken xs
getNextToken ('\v':xs)    = getNextToken xs

getNextToken ('[':xs)     = addToken LBRA    xs
getNextToken (']':xs)     = addToken RBRA    xs
getNextToken ('+':xs)     = addToken PLUS    xs
getNextToken ('-':xs)     = addToken MINUS   xs
getNextToken ('*':xs)     = addToken MULT    xs
getNextToken ('/':xs)     = addToken DIV     xs
getNextToken ('(':xs)     = addToken LPAREN  xs
getNextToken (')':xs)     = addToken RPAREN  xs
getNextToken (':':xs)     = addToken COLON   xs
getNextToken ('&':xs)     = addToken CONCAT  xs
getNextToken ('<':'=':xs) = addToken TLTE    xs
getNextToken ('>':'=':xs) = addToken TGTE    xs
getNextToken ('!':'=':xs) = addToken TNE     xs
getNextToken ('<':xs)     = addToken TLT     xs
getNextToken ('=':xs)     = addToken TEQ     xs
getNextToken ('>':xs)     = addToken TGT     xs
getNextToken ('.':xs)     = addToken DOT     xs
getNextToken (',':xs)     = addToken COMMA   xs

getNextToken ('\'':xs)    = getStr xs

getNextToken str@(x:_)
    | isDigit x     = getNum str
    | isId x        = getId str >>= filterKeyword

getNextToken (x:_)  = tokenizeFail $ "umatched token " ++ (show x)
getNextToken []     = return []

addToken :: Token -> String -> Runtime [Token]
addToken token rest = (token:) `liftM` getNextToken rest

getNum :: String -> Runtime [Token]
getNum str = addToken (INT (read numStr)) rest
    where (numStr, rest) = span isDigit str

getId :: String -> Runtime [Token]
getId (x:xs) = addToken (ID (x:idTail)) rest
    where (idTail, rest) = span isCharId xs
          isCharId char = isId char || isDigit char
getId _ = error "only non-empty lists are accepted"

filterKeyword :: [Token] -> Runtime [Token]
filterKeyword ((def@(ID name)):rest) = return (newToken:rest)
    where newToken = Map.findWithDefault def name keywords
filterKeyword _ = error "only new ID tokens are accepted"

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
getStr [] = isEOF >>= ifIsEOF
    where ifIsEOF True  = tokenizeFail $ "unterminated string"
          ifIsEOF False = readLine >>= addChar '\n'

parseInt :: Int -> String -> Int
parseInt radix = foldl (\acc x -> acc * radix + digitToInt x) 0

addChar :: Char -> String -> Runtime [Token]
addChar char rest = getStr rest >>= prepend
    where prepend ((STR str):tokens) = return ((STR (char:str)):tokens)
          prepend _ = error "only new string tokens are accepted"
