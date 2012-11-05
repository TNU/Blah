module Runner (
    runLine,
) where

import Control.Monad (liftM, liftM2)
import Control.Monad.Trans.Error (runErrorT)

import qualified Data.Map as Map

import Parser
import Value
import State
import Eval

{- Run Functions -}
runLine :: Line -> (Value -> Runtime ()) -> Runtime ()
runLine (Ls stmt)        sep = runStmt stmt sep
runLine (Lm line stmt)   sep = runLine line sep >> runStmt stmt sep

runStmt :: Stmt -> (Value -> Runtime ()) -> Runtime ()
runStmt (Se expr)        sep = runExpr expr >>= sep
runStmt (Assn name expr) sep = runExpr expr >>= runAssign name sep
runStmt (Si ifStmt)      sep = runIfStmt ifStmt sep
runStmt (Sw whileStmt)   sep = runWhileStmt whileStmt sep

runAssign :: String -> (Value -> Runtime ()) -> Value -> Runtime ()
runAssign name sep value = setVar name value >> doNothing

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

runExpr :: Expr -> Runtime Value
runExpr expr = snd `liftM` evalExpr expr

testExpr :: Expr -> Runtime Bool
testExpr expr = fst `liftM` evalExpr expr

doNothing :: Runtime ()
doNothing = return ()
