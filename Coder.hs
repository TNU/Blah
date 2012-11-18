module Coder (
    runRepl
) where

import Control.Monad.Trans.State (evalState)
import Control.Monad.Trans.Error (catchError)

import qualified Data.Map as Map

import State
import Tokenizer (tokenize)
import Parser (parse, Decl(..))
import Converters (toStr)
import Runner (runLine)

runRepl :: IO ()
runRepl = run repl replRuntime >> return ()
    where replRuntime = newRuntime Map.empty

repl :: Runtime ()
repl = replRest

replRest :: Runtime ()
replRest = replLine []

replLine :: [Decl] -> Runtime ()

replLine [(Dl line)]  = replError (runLine line replShowLine) >> replRest
    where replError state = state `catchError` showLine
          replShowLine val = toStr val >>= showLine

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

replFail :: String -> Runtime ()
replFail = showLine . ("<repl> " ++)

