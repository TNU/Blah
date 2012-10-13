module Coder (
    repl
) where

import Control.Monad (liftM2)
import Control.Monad.Error
import Control.Monad.State

import Tokenizer (tokenize)
import Parser (parse, Decl(..))
import Runner
           
type Failable = Either String           
           
repl :: IO ()
repl = getContents >>= replRun

replRun :: String -> IO ()
replRun input = sequence_ $ evalState replRunRest (newRuntime input)

replRunRest :: State Runtime [IO ()]
replRunRest = replRunLine (return [])

replRunLine :: Failable [Decl] -> State Runtime [IO ()]
replRunLine (Left error)        = replPrint error 
replRunLine (Right [(De expr)]) = evalExpr expr >>= showValOrErr
replRunLine (Right [(Ds stmt)]) = evalStmt stmt >>= showOnlyErr
replRunLine (Right unmatched)   = do 
        input <- getInput
        case (unmatched, input) of
            ([], []) -> return []
            (xs, []) -> return [replFail "unmatched decls at end of input"]
            _     -> do let (tokens, restInput) = tokenize input
                            decls = tokens >>= parse unmatched
                        updateInput restInput
                        replRunLine decls
                        
showOnlyErr :: Failable () -> State Runtime [IO ()]
showOnlyErr (Right ())    = replRunRest
showOnlyErr (Left error)  = printStr error replRunRest
 
showValOrErr :: Failable Value -> State Runtime [IO ()]
showValOrErr (Right val) = printVal val replRunRest
showValOrErr (Left error) = printStr error replRunRest
 
replPrint :: String -> State Runtime [IO ()]
replPrint str = printStr str replRunRest
 
-- error handling
replFail :: String -> IO ()
replFail = putStrLn . ("<repl> " ++)
