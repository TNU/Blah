module Runner (
    runLine,
) where

import Control.Monad (liftM, liftM2)
import Control.Monad.Trans.Error (runErrorT)

import qualified Data.Map as Map

import Parser
import State
import Eval

{- Run Functions -}
runLine :: Line -> (Value -> Runtime ()) -> Runtime ()
runLine (Ls stmt)        sep = runStmt stmt sep
runLine (Lm line stmt)   sep = runLine line sep >> runStmt stmt sep

runStmt :: Stmt -> (Value -> Runtime ()) -> Runtime ()
runStmt (Se expr)        sep = runExpr expr >>= sep
runStmt (Sa assnStmt)    sep = runAssign assnStmt sep
runStmt (Si ifStmt)      sep = runIfStmt ifStmt sep
runStmt (Sw whileStmt)   sep = runWhileStmt whileStmt sep

runAssign :: AssnStmt -> (Value -> Runtime ()) -> Runtime ()
runAssign (AssnId name expr) sep = runExpr expr >>= setVar name >> doNothing
runAssign (AssnElem elem expr) sep = runExpr expr >>=elemAssn elem >> doNothing

runIfStmt :: IfStmt -> (Value -> Runtime ()) -> Runtime ()
runIfStmt (If test line) sep = testExpr test >>= decider
    where decider True  = runLine line sep
          decider False = doNothing
runIfStmt (IfOther test ifLines otherLine) sep = testExpr test >>= decider
    where decider True  = runLine ifLines   sep
          decider False = runLine otherLine sep
runIfStmt (IfElse test ifLines elseIfStmt) sep = testExpr test >>= decider
    where decider True  = runLine ifLines      sep
          decider False = runIfStmt elseIfStmt sep

runWhileStmt :: WhileStmt -> (Value -> Runtime ()) -> Runtime ()
runWhileStmt stmt@(While test lines) sep = testExpr test >>= decider
    where decider True  = runLine lines sep >> runWhileStmt stmt sep
          decider False = doNothing

doNothing :: Runtime ()
doNothing = return ()
