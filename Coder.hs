module Coder (
    repl
) where

import Control.Monad (liftM2)
import Control.Monad.Trans.Error
import Control.Monad.Trans.State

import Failure
import Tokenizer (tokenize)
import Parser (parse, Decl(..))
import Runner

repl :: IO ()
repl = getContents >>= replRun

replRun :: String -> IO ()
replRun input = evalState replRunRest (newRuntime input)

replRunRest :: RuntimeIO ()
replRunRest = replRunLine (return [])

replRunLine :: Either Failure [Decl] -> RuntimeIO ()
replRunLine (Left error)        = showRawError error
replRunLine (Right [(Ds stmt)]) = evalStmt replRunRest stmt
replRunLine (Right unmatched)   = do
        input <- getInput
        case (unmatched, input) of
            ([], []) -> return . return $ ()
            (xs, []) -> return . replError $ "unmatched decls at end of input"
            _     -> do let (tokens, restInput) = tokenize input
                            decls = tokens >>= parse unmatched
                        updateInput restInput
                        replRunLine (extract decls)

showRawError :: String -> RuntimeIO ()
showRawError str = showStr replRunRest str
