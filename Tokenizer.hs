module Tokenizer (
    Token(..),
    tokenize,
) where

import Data.List (span)
import Control.Monad (liftM2)
import Control.Monad.Error

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
              
tokenize :: (Error e, MonadError e m) => String -> (m [Token], String)
tokenize [] = (return [], "")
tokenize (x:xs) 
    | isNewline x   = (return [], xs)
    | isSpace x     = tokenize xs
tokenize str = (curTokens, restLines)
        where (token, rest) = getOneToken str
              (restToks, restLines) = tokenize rest
              curTokens = liftM2 (:) token restToks
                  
getOneToken :: (Error e, MonadError e m) => String -> (m Token, String)
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
          
getNum ::  (Error e, MonadError e m) => String -> (m Token, String)
getNum str = (return (INT (read numStr)), rest)
             where (numStr, rest) = span isNum str
             
getId ::  (Error e, MonadError e m) => String -> (m Token, String)
getId (x:xs) = (return (ID (x:idTail)), rest)
               where (idTail, rest) = span isNumOrLetter xs
                     isNumOrLetter x = (isNum x) || (isLetter x)

-- error handling
tokenizeFail :: (Error e, MonadError e m) => String -> m a
tokenizeFail = throwError . strMsg . ("<tokenize> " ++)

