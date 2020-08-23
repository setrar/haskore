{- | Identifier generator

Generates unique elements but elements can be declared as unused, again.

-}

module Haskore.General.IdGenerator where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.State(State, state, evalState, modify, get, )
import Control.Monad (when, )
import Data.Tuple.HT (mapFst, )

{- |

The generator is a state monad
where the state consists of the set of the explicitly unused elements
and a lower bound for another set of ids that are still unused.
Essentially, the Set stores all recycled ids,
and the lower bound stores the ids not used so far.
All elements in the explicit set must be below the bound.

-}

type T i a = State (St i) a

type St i = (Set i, i)

run :: i -> T i a -> a
run start gen = evalState gen (Set.empty, start)

{- |

Reserve a new id.

-}

alloc :: (Ord i, Enum i) => T i i
alloc =
   state $ \(set, next) ->
      if Set.null set
        then (next, (set, succ next))
        else let (newId, newSet) = Set.deleteFindMin set
             in  (newId, (newSet, next))

{- |

Return an id.

We call reduce in order to prevent the set from growing too much.
We call it only once in order to prevent a heavy CPU lead
when the last id of a sequence is returned.
So the reduction is spread over several calls to 'free'.

-}

free :: (Ord i, Enum i) => i -> T i ()
free oldId =
   do s <- get
      when (isFree s oldId)
           (error "IdGenerator.free: id freed twice")
      modify (mapFst (Set.insert oldId))
      modify reduce

{- |

If the largest free id and the lower bound of free ids are successive elements
then we can decrease the lower bound.
This procedure can be iterated.
This way we can save storage in the set.

-}

reduce :: (Ord i, Enum i) => St i -> St i
reduce (set, next) =
   if not (Set.null set) && Set.findMax set == pred next
     then (Set.deleteMax set, pred next)
     else (set, next)

isFree :: (Ord i) => St i -> i -> Bool
isFree (set,next) i = Set.member i set || i >= next

isValid :: (Ord i) => St i -> Bool
isValid (set,next) = Set.findMax set < next
