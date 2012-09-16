import Data.List (intercalate)
import Control.Monad.Instances
import Tokenizer

main = interact eval

eval :: String -> String
eval = concatMap disp . tokenize 
    where disp (Left message) = message
          disp (Right tokens) = show tokens ++ "\n"