module Coder (
    runRepl
) where

import Control.Monad.Trans.State (evalState)
import Control.Monad.Trans.Error (catchError)

import qualified Data.Map as Map

import Tokenizer (tokenize)
import Parser (parse, Decl(..))
import Value (Value(..), toStr)
import State (Runtime, Scope, run, newRuntime, showLine, readLine, isEOF)
import Runner (runLine)

runRepl :: IO ()
runRepl = run repl (newRuntime defaultVars) >> return ()

repl :: Runtime ()
repl = replRest

replRest :: Runtime ()
replRest = replLine []

replLine :: [Decl] -> Runtime ()

replLine [(Dl line)]  = replError (runLine line replShowLine) >> replRest
    where replError state = state `catchError` showLine
          replShowLine = showLine . toStr

replLine (line@((Dl _):_)) = replLine (Dnewline:line)

replLine unmatched = do
        isEof <- isEOF
        if isEof
        then if null unmatched
             then return ()
             else replFail "unmatched decls at end of input"
        else parseError parseRestLine >>= replLine
    where parseRestLine = tokenize >>= parse unmatched
          parseError state = state `catchError` handleError
          handleError error = showLine error >> return []



defaultVars :: Scope
defaultVars = Map.empty

replFail :: String -> Runtime ()
replFail = showLine . ("<repl> " ++)

