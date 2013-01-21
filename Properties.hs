module Properties (
    valProp
) where

import qualified Data.Sequence as Seq

import Data.List
import Data.Char

import State

valProp :: String -> Value -> Runtime Value
valProp prop (Vs str)   = strProp prop str
valProp prop (Vrl ref)  = listProp prop ref
valProp _    x          = typeFail1 "property lookup" x

strProp :: String -> String -> Runtime Value
strProp "length"    str = return . Vi . length $ str
strProp "count"     str = return (Vsf (strCount str) (str ++ ".count"))
strProp "find"      str = return (Vsf (strFind str) (str ++ ".find"))
strProp "substring" str = return (Vsf (strSubstring str) (str ++ ".substring"))
strProp "toUpper"   str = return (Vsf (strToUpper str) (str ++ ".toUpper"))
strProp "toLower"   str = return (Vsf (strToLower str) (str ++ ".toLower"))
strProp x   _  = evalFail $ "string does not have the property \"" ++ x ++ "\""

strCount :: String -> [Value] -> Runtime Value
strCount str [(Vi t), (Vi f), (Vs s)] = strCount (take t str) [(Vi f), (Vs s)]
strCount str [(Vi from), (Vs sub)] = strCount (drop from str) [(Vs sub)]
strCount str [(Vs substr)] = return . Vi . count str $ 0
    where sublen = length substr
          count [] n = n
          count string n
            | substr `isPrefixOf` string = count (drop sublen string) (n + 1)
            | otherwise = count (tail string) n
strCount _   _  = argFail $ "string.count([string substring, [int from," ++
                                                             "int to]])"
strFind :: String -> [Value] -> Runtime Value
strFind str [(Vi to), (Vi f), (Vs s)] = strFind (take to str) [(Vi f), (Vs s)]
strFind str [(Vi from), (Vs s)] = strFind (drop from str) [(Vs s)] >>= addFrom
    where addFrom v@(Vi (-1)) = return v
          addFrom (Vi i)      = return (Vi (i + from))
          addFrom _  = error "strFind should only have returned an integer"
strFind str [(Vs substr)] = return . Vi . maybeToInt $ maybeIndex
    where maybeIndex = findIndex (substr `isPrefixOf`) (tails str)
          maybeToInt (Just int) = int
          maybeToInt Nothing    = -1
strFind _      _             = argFail $ "string.find([string substring," ++
                                                      "[int from, [int to]]])"

strSubstring :: String -> [Value] -> Runtime Value
strSubstring string [(Vi j), (Vi i)] = return . Vs . drop i . take j $ string
strSubstring string [(Vi i)] = return . Vs . drop i $ string
strSubstring string [] = return . Vs $ string
strSubstring _      _  = argFail $ "string.substring([int from, [int to]])"

strToLower :: String -> [Value] -> Runtime Value
strToLower str [] = return . Vs . map toLower $ str
strToLower _   _  = argFail $ "string.toLower()"

strToUpper :: String -> [Value] -> Runtime Value
strToUpper str [] = return . Vs . map toUpper $ str
strToUpper _   _  = argFail $ "string.toUpper()"

listProp :: String -> Int -> Runtime Value
listProp "length" i = listExtract i >>= return . Vi . Seq.length
listProp "push"   i = return (Vbsf listPush i "list.push");
listProp "pop"    i = return (Vbsf listPop i "list.pop");
listProp x      _ = evalFail $ "list does not have the property \""++ x ++ "\""

listExtract :: Int -> Runtime (Seq.Seq Value)
listExtract i = getFromHeap i >>= valToList
    where valToList (Vl list) = return list
          valToList _ = error "reference should only refer to a list"

listPush :: Int -> [Value] -> Runtime Value
listPush i args = do (Vl oldList) <- getFromHeap i
                     let newList = foldr (flip (Seq.|>)) oldList args
                     setAtHeap i (Vl newList)
                     return Vnothing

listPop :: Int -> [Value] -> Runtime Value
listPop i [] = do (Vl oldList) <- getFromHeap i
                  let len = Seq.length oldList
                  if len == 0
                  then evalFail "popping an empty list"
                  else do let (rest, popped) = Seq.splitAt (len - 1) oldList
                          setAtHeap i (Vl rest)
                          return (Seq.index popped 0)
listPop _ _  = argFail $ "list.pop()"
