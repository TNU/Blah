module Coder (
    repl
) where

import Control.Monad.Error

import Tokenizer
import Parser
           
type Failable = Either String

data Value = Vi Integer
           deriving (Show)

repl :: IO ()
repl = getContents >>= replRun

replRun :: String -> IO ()
replRun = sequence_ . replRunRest

replRunLine :: Either String [Decl] -> String -> [IO ()]
replRunLine (Left error)            input = (putStrLn error):(replRunRest input)
replRunLine (Right [expr@(De _)])   input = (print (evalExpr expr input)):(replRunRest input)
replRunLine (Right [])              []    = []
replRunLine (Right unmatched)       []    = [replFail "unmatched decls at end of input"]
replRunLine (Right unmatched)       input = replRunLine newDecls restInput
        where  (tokens, restInput) = tokenize input
               newDecls = tokens >>= parse unmatched

replRunRest :: String -> [IO ()]
replRunRest = replRunLine (return [])
               
evalExpr :: Decl -> String -> Value
evalExpr decls input = Vi 1

-- error handling
replFail :: String -> IO ()
replFail = putStrLn . ("<repl> " ++)
