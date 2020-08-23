{- |
For use in Tree and Graph modules.
-}
module Haskore.General.TagDictionary (T, empty, insert, lookup, singleton) where

import Haskore.General.Map (Map, empty, singleton)
import qualified Haskore.General.Map as Map

import Prelude hiding (lookup)


type T tag tree = Map tag tree

insert :: Ord tag => tag -> tree -> Map tag tree -> Map tag tree
insert =
   Map.insertWith
      (error "TagDictionary.insert: multiple definition of tag")

lookup :: (Ord tag) => Map tag tree -> tag -> tree
lookup dict =
   Map.findWithDefault dict (error "unknown loop tag")
