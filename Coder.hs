module Coder (
    repl
) where

import Control.Monad (liftM2)
import Control.Monad.Trans.Error
import Control.Monad.Trans.State

import Failure
import Tokenizer (tokenize)
import Parser (parse, Decl(..), Line(..), Stmt(..))
import Runner

repl :: IO ()
repl = getContents >>= replRun

replRun :: String -> IO ()
replRun input = evalState replRunRest (newRuntime input)

replRunRest :: RuntimeIO ()
replRunRest = replRunLine (return [])

replRunLine :: Either Failure [Decl] -> RuntimeIO ()
replRunLine (Left error)        = showRawError error
replRunLine (Right [(Dl line)]) = replLine replRunRest line
replRunLine (Right unmatched)   = do
        input <- getInput
        case (unmatched, input) of
            ([], []) -> return . return $ ()
            (xs, []) -> return . replError $ "unmatched decls at end of input"
            _     -> do let (tokens, restInput) = tokenize input
                            decls = tokens >>= parse unmatched
                        updateInput restInput
                        replRunLine (extract decls)

replLine :: RuntimeIO () -> Line -> RuntimeIO ()
replLine doRest (Ls stmt)       = replStmt doRest stmt
replLine doRest (Lm line stmt)  = replLine (replStmt doRest stmt) line

replStmt :: RuntimeIO () -> Stmt -> RuntimeIO ()
replStmt doRest (Assn name expr) = runExpr expr >>= replAssign doRest name
replStmt doRest (Se expr) = runExpr expr >>= showValOrErr doRest

replAssign :: RuntimeIO () -> String -> Either Failure Value -> RuntimeIO ()
replAssign doRest name (Right val)  = runErrorT (setVar name val) >>= showOnlyErr doRest
replAssign doRest name (Left error) = showStr doRest error

showRawError :: String -> RuntimeIO ()
showRawError str = showStr replRunRest str