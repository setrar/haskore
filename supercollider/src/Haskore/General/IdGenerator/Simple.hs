{- | Identifier generator

Generates unique elements without recycling of identifiers.

-}

module Haskore.General.IdGenerator.Simple where

import Control.Monad.Trans.State (State, evalState, modify, get, )
import Control.Monad (when, )

{- |

The generator is a simple state monad.

-}

type T i a = State i a

run :: i -> T i a -> a
run = flip evalState

{- |

Reserve a new id.

-}

alloc :: (Enum i) => T i i
alloc =
   do newId <- get
      modify succ
      return newId

{- |

Unreserve an id.
In this implementation it performs essentially nothing.

-}

free :: (Ord i) => i -> T i ()
free oldId =
   do freeId <- get
      when (freeId <= oldId)
           (error "IdGeneratorSimple.free: freed unreserved id")
