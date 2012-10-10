module Coder (
    repl
) where

import Control.Monad (liftM2)
import Control.Monad.Error
import Control.Monad.State

import Tokenizer (tokenize)
import Parser (parse, Decl(..))
import Runner
           
type Failable = Either String           
           
repl :: IO ()
repl = getContents >>= replRun

replRun :: String -> IO ()
replRun input = sequence_ $ evalState replRunRest input

replRunRest :: State Runtime [IO ()]
replRunRest = replRunLine (return [])

replRunLine :: Failable [Decl] -> State Runtime [IO ()]
replRunLine (Left error)        = replPrint error 
replRunLine (Right [(De expr)]) = evalExpr expr >>= safePrintVal
replRunLine (Right unmatched)   = do 
        input <- getInput
        case (unmatched, input) of
            ([], []) -> return []
            (xs, []) -> return [replFail "unmatched decls at end of input"]
            _     -> do let (tokens, restInput) = tokenize input
                            decls = tokens >>= parse unmatched
                        putInput restInput
                        replRunLine decls
 
safePrintVal :: Failable Value -> State Runtime [IO ()]
safePrintVal (Right val) = printVal val replRunRest
safePrintVal (Left error) = printStr error replRunRest
 
replPrint :: String -> State Runtime [IO ()]
replPrint str = printStr str replRunRest
 
-- error handling
replFail :: String -> IO ()
replFail = putStrLn . ("<repl> " ++)
