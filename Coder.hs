module Coder (
    repl
) where

import Control.Monad (liftM2)
import Control.Monad.Error
import Control.Monad.State

import Tokenizer (tokenize)
import Parser (parse, Decl(..))
import Runner        
           
repl :: IO ()
repl = getContents >>= replRun

replRun :: String -> IO ()
replRun input = sequence_ $ evalState replRunRest (newRuntime input)

replRunRest :: State Runtime [IO ()]
replRunRest = replRunLine (return [])

replRunLine :: Failable [Decl] -> State Runtime [IO ()]
replRunLine (Left error)        = showRawError error
replRunLine (Right [(Ds stmt)]) = evalStmt replRunRest stmt 
replRunLine (Right unmatched)   = do 
        input <- getInput
        case (unmatched, input) of
            ([], []) -> return []
            (xs, []) -> return [replFail "unmatched decls at end of input"]
            _     -> do let (tokens, restInput) = tokenize input
                            decls = tokens >>= parse unmatched
                        updateInput restInput
                        replRunLine decls
 
showRawError :: String -> State Runtime [IO ()]
showRawError str = showStr replRunRest str
 
-- error handling
replFail :: String -> IO ()
replFail = putStrLn . ("<repl> " ++)
