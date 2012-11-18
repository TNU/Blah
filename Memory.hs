module Memory (
    Memory,
    insert,
    update,
    extract,
    hasIndex,
    newMemory,
) where

import qualified Data.Sequence as Seq

data Memory v = Memory (Seq.Seq v) Int

newMemory = Memory Seq.empty 0

insert :: Memory v -> v -> (Memory v, Int)
insert (Memory items uses) value = (Memory newItems newNumUses, newIndex)
    where newIndex = Seq.length items
          newItems = items Seq.|> value
          newNumUses = uses + 1

update :: Memory v -> Int -> v -> Memory v
update (Memory items uses) index value = Memory newItems newNumUses
    where newItems = Seq.update index value items
          newNumUses = uses + 1

hasIndex :: Memory v -> Int -> Bool
hasIndex (Memory items uses) index = index < Seq.length items

extract :: Memory v -> Int -> v
extract (Memory items uses) index = Seq.index items index
