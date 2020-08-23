module Haskore.General.Map
       (Map, (!), (\\), null, size, member, lookup, findWithDefault,
        empty, singleton,
        insert, insertWith, insertWithKey, insertLookupWithKey,
        delete, adjust, adjustWithKey,
        update, updateWithKey, updateLookupWithKey,
        union, unionWith, unionWithKey, unions, unionsWith,
        difference, differenceWith, differenceWithKey,
        intersection, intersectionWith, intersectionWithKey,
        map, mapWithKey, mapAccum, mapAccumWithKey,
        mapKeys, mapKeysWith, mapKeysMonotonic,
        fold, foldWithKey, elems, keys, keysSet,
        assocs, toList, fromList, fromListWith, fromListWithKey,
        toAscList, fromAscList, fromAscListWith, fromAscListWithKey,
        fromDistinctAscList, filter, filterWithKey,
        partition, partitionWithKey, split, splitLookup,
        isSubmapOf, isSubmapOfBy, isProperSubmapOf, isProperSubmapOfBy,
        lookupIndex, findIndex, elemAt, updateAt, deleteAt,
        findMin, findMax, deleteMin, deleteMax, deleteFindMin, deleteFindMax,
        updateMin, updateMax, updateMinWithKey, updateMaxWithKey,
        showTree, showTreeWith, valid)
       where

import qualified Data.Map as Map
import Data.Map
   (Map, (!), (\\), null, size, member, empty, singleton,
    insert, insertWith, insertWithKey, insertLookupWithKey,
    delete, adjust, adjustWithKey,
    update, updateWithKey, updateLookupWithKey,
    union, unionWith, unionWithKey, unions, unionsWith,
    difference, differenceWith, differenceWithKey,
    intersection, intersectionWith, intersectionWithKey,
    map, mapWithKey, mapAccum, mapAccumWithKey,
    mapKeys, mapKeysWith, mapKeysMonotonic,
    fold, foldWithKey, elems, keys, keysSet,
    assocs, toList, fromList, fromListWith, fromListWithKey,
    toAscList, fromAscList, fromAscListWith, fromAscListWithKey,
    fromDistinctAscList, filter, filterWithKey,
    partition, partitionWithKey, split, splitLookup,
    isSubmapOf, isSubmapOfBy, isProperSubmapOf, isProperSubmapOfBy,
    elemAt, updateAt, deleteAt,
    findMin, findMax, deleteMin, deleteMax, deleteFindMin, deleteFindMax,
    updateMin, updateMax, updateMinWithKey, updateMaxWithKey,
    showTree, showTreeWith, valid)

import Prelude hiding (lookup, map, filter, null)

{-
  The signatures of the lookup functions in Data.Map
  are very unfortunate.
  We replace them by more usable ones here.
-}

lookup :: Ord k => Map k a -> k -> Maybe a
lookup = flip Map.lookup

findWithDefault :: Ord k => Map k a -> a -> k -> a
findWithDefault dict deflt key =
   Map.findWithDefault deflt key dict

lookupIndex :: Ord k => Map k a -> k -> Maybe Int
lookupIndex = flip Map.lookupIndex

findIndex :: Ord k => Map k a -> k -> Int
findIndex = flip Map.findIndex
