module Value (
    Value(..),
) where

import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

data Value = Vi Integer
           | Vb Bool
           | Vs String
           | Vl (Seq.Seq Value)
           | Vf String
           | Vrl Int
           | Vnothing
           deriving (Ord, Eq)

instance Show Value where
    show Vnothing    = "Nothing"
    show (Vi int)    = show int
    show (Vb bool)   = show bool
    show (Vs string) = string
    show (Vf name)   = name ++ "(..)"
    show (Vrl index) = "LIST REF: " ++ (show index) ++ ""
    show (Vl list)   = show (Fold.toList list)
