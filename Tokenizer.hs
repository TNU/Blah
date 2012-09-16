module Tokenizer (
    Token(..),
    tokenize
) where

import Data.List (span)
import Control.Monad (liftM2)

data Token = INT Integer 
           | ID String
           | OP Char Int 
           deriving (Show)
           
isNewline x = x `elem` "\n\r"
isSpace x   = x `elem` " \f\t\v"
isNum x     = x `elem` ['0'..'9']
isLetter x  = x `elem` (['a'..'z'] ++ ['A'..'Z'])
isOp x      = x `elem` "+-*/"
           
tokenize :: (Monad m) => String -> [m [Token]]
tokenize [] = [(return [])]
tokenize (x:xs) 
    | isNewline x   = ((return []) : (tokenize xs))
    | isSpace x     = tokenize xs
tokenize str = newCurLine : otherLines
            where (token, rest) = getOneToken str
                  (curLine:otherLines) = tokenize rest
                  newCurLine = liftM2 (:) token curLine
                  
getOneToken :: (Monad m) => String -> (m Token, String)
getOneToken str@(x:xs)
    | isNum x       = getNum str
    | isLetter x    = getId str
    | isOp x        = getOp str
    | otherwise     = (error ("TOKENIZE: umatched token '" ++ (show x) ++ "'"), xs)
          
getNum :: (Monad m) => String -> (m Token, String)
getNum str = (return (INT (read numStr)), rest)
             where (numStr, rest) = span isNum str
             
getId :: (Monad m) => String -> (m Token, String)
getId (x:xs) = (return (ID (x:idTail)), rest)
               where (idTail, rest) = span isNumOrLetter xs
                     isNumOrLetter x = (isNum x) || (isLetter x)
                
getOp :: (Monad m) => String -> (m Token, String)
getOp (x:rest)
    | x `elem` "+-" = (return (OP x 1), rest)
    | x `elem` "*/" = (return (OP x 2), rest)
