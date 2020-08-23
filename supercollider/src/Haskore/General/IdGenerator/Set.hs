{- | Identifier generator

Generates unique elements but elements can be declared as unused, again.

-}

module Haskore.General.IdGenerator.Set where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.RWS(RWS, evalRWS, modify, get, ask, )
import Control.Monad (when, )

{- |

The generator is a reader+state monad
where the state consists of the allocated identifiers
and the reader source is the least element ever used.

-}

type T i a = RWS i () (Set i) a

run :: i -> T i a -> a
run start gen = fst $ evalRWS gen start Set.empty

{- |

Reserve a new id.

-}

alloc :: (Ord i, Enum i) => T i i
alloc =
   do start <- ask
      allocated <- get
      let newId = head $ filter (not . flip Set.member allocated) [start ..]
      modify (Set.insert newId)
      return newId

{- |

Deallocate an id.

-}

free :: (Ord i, Enum i) => i -> T i ()
free oldId =
   do allocated <- get
      when (not $ Set.member oldId allocated)
           (error "IdGeneratorSet.free: deallocated free identifier")
      modify (Set.delete oldId)
