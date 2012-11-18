module Converters (
    toBool,
    toStr,
    toRepr,
    deref,
) where

import Control.Monad (liftM, liftM2)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

import State

toBool :: Value -> Runtime Bool
toBool Vnothing     = return False
toBool (Vb x)       = return x
toBool (Vi 0)       = return False
toBool (Vi x)       = return True
toBool (Vs "")      = return False
toBool (Vs x)       = return True
toBool (Vl x)       = return . not . Seq.null $ x
toBool (Vrl i)      = getFromHeap i >>= toBool
toBool (Vsf _ _)    = return True
toBool (Vbsf _ _ _) = return True

toStr :: Value -> Runtime String
toStr Vnothing          = return "Nothing"
toStr (Vb bool)         = return . show $ bool
toStr (Vi int)          = return . show $ int
toStr (Vs string)       = return string
toStr (Vl list)         = listToRepr list
toStr (Vrl index)       = getFromHeap index >>= toStr
toStr (Vsf _ name)      = return $ name ++ "(..)"
toStr (Vbsf _ i name)   = return $ (show i) ++ ":" ++ name ++ "(..)"

toRepr :: Value -> Runtime String
toRepr Vnothing     = return "Nothing"
toRepr (Vi int)     = return . show $ int
toRepr (Vb bool)    = return . show $ bool
toRepr (Vl list)    = listToRepr list
toRepr (Vrl index)  = getFromHeap index >>= toRepr
toRepr (Vsf _ name) = return $ name ++ "(..)"
toRepr (Vbsf _ i name) = return $ (show i) ++ ":" ++ name ++ "(..)"
toRepr (Vs string)  = return $ "'" ++ concatMap esc string ++ "'"
    where esc '\a' = "\\a"
          esc '\b' = "\\b"
          esc '\f' = "\\f"
          esc '\n' = "\\n"
          esc '\r' = "\\r"
          esc '\t' = "\\t"
          esc '\v' = "\\v"
          -- '"' is not escaped
          esc '\'' = "\\\'"
          esc '\\' = "\\\\"
          esc '\0' = "\\0"
          esc x    = [x]

listToRepr :: Seq.Seq Value -> Runtime String
listToRepr seq = addBrackets `liftM` elemsToRepr
    where addBrackets elems = "[" ++ elems ++ "]"
          elemsToRepr   = Fold.foldr (liftM2 (++)) (return "") eachToRepr
          eachToRepr    = Seq.mapWithIndex oneToRepr seq
          oneToRepr 0 x = toRepr x
          oneToRepr _ x = (", " ++) `liftM` toRepr x

deref :: Value -> Runtime Value
deref (Vrl i) = getFromHeap i
deref value   = return value

