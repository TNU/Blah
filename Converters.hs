module Converters (
    toBool,
    toStr,
    toRepr,
    deref,
) where

import Control.Monad (liftM, liftM2)
import qualified Data.Set as Set
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
toStr value = visit oneToStr (Set.empty) value

oneToStr :: Value -> Runtime String
oneToStr Vnothing          = return "Nothing"
oneToStr (Vb bool)         = return . show $ bool
oneToStr (Vi int)          = return . show $ int
oneToStr (Vs string)       = return string
oneToStr (Vsf _ name)      = return $ name ++ "(..)"
oneToStr (Vbsf _ i name)   = return $ (show i) ++ ":" ++ name ++ "(..)"

toRepr :: Value -> Runtime String
toRepr value = visit oneToRepr (Set.empty) value

oneToRepr :: Value -> Runtime String
oneToRepr Vnothing     = return "Nothing"
oneToRepr (Vi int)     = return . show $ int
oneToRepr (Vb bool)    = return . show $ bool
oneToRepr (Vsf _ name) = return $ name ++ "(..)"
oneToRepr (Vbsf _ i name) = return $ (show i) ++ ":" ++ name ++ "(..)"
oneToRepr (Vs string)  = return $ "'" ++ concatMap esc string ++ "'"
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

visit :: (Value -> Runtime String) -> Set.Set Int -> Value -> Runtime String
visit action seen (Vl list) = addBrackets `liftM` elemsToRepr
    where addBrackets elems = "[" ++ elems ++ "]"
          elemsToRepr   = Fold.foldr (liftM2 (++)) (return "") eachToRepr
          eachToRepr    = Seq.mapWithIndex oneToRepr list
          oneToRepr 0 x = visit toRepr seen x
          oneToRepr _ x = (", " ++) `liftM` visit toRepr seen x
visit action seen (Vrl index)
    | Set.member index seen = return "[...]"
    | otherwise             = getFromHeap index >>= visit action newSeen
        where newSeen = Set.insert index seen
visit action seen value = action value

deref :: Value -> Runtime Value
deref (Vrl i) = getFromHeap i
deref value   = return value

