module Tokenizer (
    Token(..),
    tokenize
) where

import Data.List (span)

data Token = INT Integer 
           | ID String
           | OP Char Int 
           deriving (Show)
           
tokenize :: (Monad m) => String -> [m [Token]]
tokenize [] = [(return [])]
tokenize (x:xs) 
    | isNL x = ((return []) : (tokenize xs))
    | isSP x = tokenize xs
               
tokenize str = do
            (token, rest) <- getFirstToken str
            curLineTokens <- (head (tokenize rest))
            (return (token:curLineTokens)) : (tail (tokenize rest))
            
isNL x  = x `elem` "\n\r"
isSP x  = x `elem` " \f\t\v"

getFirstToken :: (Monad m) => String -> m (Token, String)
getFirstToken str@(x:xs)
    | isNum x   = return $ getNum str
    | isId x    = return $ getId str
    | isOP x    = return $ getOp str
    | otherwise = error ("TOKENIZE: umatched token '" ++ (show x) ++ "'")

isNum x = x `elem` ['0'..'9']
isId x  = x `elem` (['a'..'z'] ++ ['A'..'Z'])
isOP x  = x `elem` "+-*/"
          
getNum :: String -> (Token, String)
getNum str = (INT (read numStr), rest)
             where (numStr, rest) = span isNum str
             
getId :: String -> (Token, String)
getId (x:xs) = (ID (x:idTail), rest)
               where (idTail, rest) = span isNumOrId xs
                     isNumOrId x = (isNum x) || (isId x)
                
getOp :: String -> (Token, String)
getOp (x:rest)
    | x `elem` "+-" = (OP x 1, rest)
    | x `elem` "*/" = (OP x 2, rest)
          
      
