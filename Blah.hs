module Blah (
    runRepl,
    runScript
) where

import Control.Monad.Trans.Except (catchE)

import qualified System.IO as IO
import qualified Data.Map as Map

import State
import Functions
import Tokenizer
import Parser
import Decls (Decl(..))
import Converters (toStr)
import Eval (runProgram)

runRepl :: IO ()
runRepl = run repl replRuntime >> return ()
    where replRuntime = newRuntime IO.stdin sysFuncs

repl :: Runtime ()
repl = replRest

replRest :: Runtime ()
replRest = replLine []

replLine :: [Decl] -> Runtime ()

replLine [(Dl line)] = runProgram line replShowLine replRest onError
    where replShowLine val = toStr val >>= writeLine
          onError message = writeLine message >> replRest

replLine (line@((Dl _):_)) = replLine (Dnewline:line)

replLine unmatched = do
        isEof <- isEOF
        if isEof
        then if null unmatched
             then return ()
             else replFail "unmatched decls at end of input"
        else parseError parseRestLine >>= replLine
    where parseRestLine = tokenize >>= parse unmatched
          parseError state = state `catchE` handleError
          handleError (Sfailure failure) = writeLine (show failure) >> return []
          handleError signal = error $ "unexpected signal: " ++ (show signal)

replFail :: String -> Runtime ()
replFail = writeLine . ("<repl> " ++)

runScript :: IO.Handle -> IO ()
runScript file = parseScript >>= runOrError . verifyAST
    where parseScript = run scriptParse (newRuntime file Map.empty)
          runOrError (Right decls)  = runAST decls >> return ()
          runOrError (Left (Sfailure failure)) = print failure
          runOrError (Left sig) = error $ "unexpected signal: " ++ (show sig)
          runAST decls = run (script decls) (newRuntime IO.stdin sysFuncs)

scriptParse :: Runtime [Decl]
scriptParse = scriptParseLine []

scriptParseLine :: [Decl] -> Runtime [Decl]
scriptParseLine rest@((Dl _):_) = scriptParseLine (Dnewline:rest)
scriptParseLine decls = do isEof <- isEOF
                           if isEof
                           then return decls
                           else do tokens <- tokenize
                                   newDecls <- parse decls tokens
                                   scriptParseLine newDecls

-- verifies that there are only line and newline decls
verifyAST :: Either Signal [Decl] -> Either Signal [Decl]
verifyAST (Right decls)
    | all isValidDecl decls    = return decls
    | otherwise                = parseFail "invalid decl"
    where isValidDecl (Dl _)   = True
          isValidDecl Dnewline = True
          isValidDecl _        = False
          parseFail            = Left . Sfailure . Parse
verifyAST errorMessage         = errorMessage

script :: [Decl] -> Runtime ()
script []               = return ()
script (Dnewline:rest)  = script rest
script ((Dl line):rest) = runProgram line ignore (script rest) writeLine
    where ignore _ = return ()
script (decl:_)         = error $ "unexpected decl in script: " ++ (show decl)
