import Control.Monad.Instances
import Tokenizer
import Parser

main = interact eval

repl :: String -> String

eval :: String -> String
eval = concatMap disp . tokenize 
    where disp (Left message) = message
          disp (Right tokens) = show tokens ++ "\n"