module Heap (
    Heap,
    insert,
    extract,
    hasIndex,
    newHeap,
) where

import Data.Sequence as Seq

import Value

data Heap = Heap (Seq.Seq Value) Int

newHeap = Heap Seq.empty 0

insert :: Heap -> Value -> (Heap, Int)
insert (Heap items uses) value = (Heap newItems newNumUses, newIndex)
    where newItems = items Seq.|> value
          newNumUses = uses + 1
          newIndex = Seq.length items

hasIndex :: Heap -> Int -> Bool
hasIndex (Heap items uses) index = index < Seq.length items

extract :: Heap -> Int -> Value
extract (Heap items uses) index = Seq.index items index
