module Coder (
    repl
) where

import Control.Monad.Trans.State (evalState)

import Failure
import Tokenizer (tokenize)
import Parser (parse, Decl(..))
import Eval (newRuntime, getVar, setVar, getInput, updateInput)
import Runner (RuntimeIO, runLine, showStr)

repl :: IO ()
repl = getContents >>= replStr

replStr :: String -> IO ()
replStr input = evalState replRest (newRuntime input)

replRest :: RuntimeIO ()
replRest = replLine (return [])

replLine :: Either Failure [Decl] -> RuntimeIO ()
replLine (Left error)             = showStr error replRest
replLine (Right [(Dl line)])      = runLine line replRest
replLine (Right line@((Dl _):_))  = replLine (Right (Dnewline:line))
replLine (Right unmatched) = do
        input <- getInput
        case (unmatched, input) of
            ([], []) -> return . return $ ()
            (xs, []) -> return . replError $ "unmatched decls at end of input"
            _     -> do let (tokens, restInput) = tokenize input
                            decls = tokens >>= parse unmatched
                        updateInput restInput
                        replLine (extract decls)

