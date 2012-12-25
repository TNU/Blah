module Memory (
    Memory(..),
    insert,
    update,
    extract,
    hasIndex,
    numInserts,
    newMemory,
) where

import Data.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

data Memory v = Memory (Seq.Seq v) (Set.Set Int) Int

newMemory = Memory Seq.empty Set.empty 0

numInserts :: Memory v -> Int
numInserts (Memory _ _ numIns) = numIns

insert :: Memory v -> v -> (Memory v, Int)
insert (Memory items freeIndices numIns) value
    | Set.null freeIndices = (Memory nullItems Set.empty newNumIns, nullIndex)
    | otherwise            = (Memory freeItems restFree newNumIns, freeIndex)
    where nullIndex = Seq.length items
          nullItems = items Seq.|> value
          newNumIns = numIns + 1
          (freeIndex, restFree) = Set.deleteFindMin freeIndices
          freeItems = Seq.update freeIndex value items

update :: Memory v -> Int -> v -> Memory v
update (Memory items frees numIns) index value = Memory newItems frees numIns
    where newItems = Seq.update index value items

hasIndex :: Memory v -> Int -> Bool
hasIndex (Memory items freeIndices _) index = index >= 0
                                          && index < Seq.length items
                                          && not (Set.member index freeIndices)

extract :: Memory v -> Int -> v
extract (Memory items _ _) index = Seq.index items index
