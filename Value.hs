module Value (
    Value(..),
    toBool,
    toStr,
    toRepr,
) where

import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

data Value = Vi Integer
           | Vb Bool
           | Vs String
           | Vl (Seq.Seq Value)
           | Vf String
           | Vnothing
           deriving (Ord, Eq)

instance Show Value where
    show = toStr

{- Converters -}
toBool :: Value -> Bool
toBool (Vi 0)   = False
toBool (Vi x)   = True
toBool (Vb x)   = x
toBool (Vs "")  = False
toBool (Vs x)   = True
toBool (Vl x)   = not . Seq.null $ x
toBool (Vf _)   = True
toBool Vnothing = False

toStr :: Value -> String
toStr Vnothing    = "Nothing"
toStr (Vi int)    = show int
toStr (Vb bool)   = show bool
toStr (Vs string) = string
toStr (Vl list)   = toReprList list
toStr (Vf name)   = name ++ "(..)"

toRepr :: Value -> String
toRepr Vnothing  = "Nothing"
toRepr (Vi int)  = show int
toRepr (Vb bool) = show bool
toRepr (Vl list) = toReprList list
toRepr (Vf name) = name ++ "(..)"
toRepr (Vs string) = "'" ++ concatMap esc string ++ "'"
    where esc '\a' = "\\a"
          esc '\b' = "\\b"
          esc '\f' = "\\f"
          esc '\n' = "\\n"
          esc '\r' = "\\r"
          esc '\t' = "\\t"
          esc '\v' = "\\v"
          -- " is not escaped
          esc '\'' = "\\\'"
          esc '\\' = "\\\\"
          esc '\0' = "\\0"
          esc x    = [x]

toReprList :: Seq.Seq Value -> String
toReprList seq = "[" ++ toReprElems ++ "]"
    where toReprElems = Fold.concat (Seq.mapWithIndex showOne seq)
          showOne 0 x = toRepr x
          showOne _ x = ", " ++ toRepr x

