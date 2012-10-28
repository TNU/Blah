module Runner (
    RuntimeIO,
    runLine,
    showStr,
) where

import Control.Monad (liftM)
import Control.Monad.Trans.Error (runErrorT)

import Failure
import Parser
import Eval

type RuntimeIO a     = RuntimeState (IO a)
type RuntimeAction a = RuntimeIO a -> RuntimeIO a

{- Run Functions -}
runLine :: Line -> RuntimeAction a
runLine (Ls stmt)       = runStmt stmt
runLine (Lm line stmt)  = runLine line . runStmt stmt

runStmt :: Stmt -> RuntimeAction a
runStmt (Se expr)        = runExpr expr `passTo` showValOrErr
runStmt (Assn name expr) = runExpr expr `passTo` (runAssign name)
runStmt (Si ifStmt)      = runIfStmt ifStmt
runStmt (Sw whileStmt)   = runWhileStmt whileStmt

runAssign :: String -> Either Failure Value -> RuntimeAction a
runAssign name (Right val)  = runErrorT (setVar name val) `passTo` showOnlyErr
runAssign name (Left error) = showStr error

runIfStmt :: IfStmt -> RuntimeAction a
runIfStmt (If test line) = testExpr test `passTo` \result ->
        case result of (Right True)  -> runLine line
                       (Right False) -> id
                       (Left error)  -> showStr error
runIfStmt (IfOther test ifLines otherLine) = testExpr test `passTo` \result ->
        case result of (Right True)  -> runLine ifLines
                       (Right False) -> runLine otherLine
                       (Left error)  -> showStr error
runIfStmt (IfElse test ifLines elseIfStmt) = testExpr test `passTo` \result ->
        case result of (Right True)  -> runLine ifLines
                       (Right False) -> runIfStmt elseIfStmt
                       (Left error)  -> showStr error

runWhileStmt :: WhileStmt -> RuntimeAction a
runWhileStmt stmt@(While test lines) = testExpr test `passTo` \result ->
        case result of (Right True)  -> runLine lines . runWhileStmt stmt
                       (Right False) -> id
                       (Left error)  -> showStr error

{- Show Functions -}
showValOrErr :: Either Failure Value -> RuntimeAction a
showValOrErr (Right val)  = showVal val
showValOrErr (Left error) = showStr error

showOnlyErr :: Either Failure () -> RuntimeAction a
showOnlyErr (Right ()) = id
showOnlyErr (Left error) = showStr error

showVal :: Value -> RuntimeAction a
showVal (Vi int)  = showRaw int
showVal (Vb bool) = showRaw bool

showRaw :: (Show t) => t -> RuntimeAction a
showRaw = showStr . show

showStr :: String -> RuntimeAction a
showStr = doIO . putStrLn

{- Expr -}
runExpr :: Expr -> RuntimeState (Either Failure Value)
runExpr expr = runErrorT (evalExpr expr) >>= return . liftM snd

testExpr :: Expr -> RuntimeState (Either Failure Bool)
testExpr expr = runExpr expr >>= return . (liftM toBool)

passTo :: RuntimeState a -> (a -> RuntimeAction b) -> RuntimeAction b
passTo value action doRest = value >>= \x -> action x doRest

doIO :: IO b -> RuntimeAction a
doIO action = (>>= return . (action >>))
