{- | Identifier generator

A simple test of whether identifiers can be re-used in SuperCollider.
This has no practical use.

-}

module Haskore.General.IdGenerator.Modulo where

import Control.Monad.Trans.State (State, evalState, modify, get, )

{- |

The generator is a simple state monad.

-}

type T i a = State i a

run :: i -> T i a -> a
run = flip evalState

{- |

Reserve a new id.

-}

alloc :: (Integral i) => T i i
alloc =
   do newId <- get
      modify (flip mod 50 . succ)
      return (newId+2)

{- |

Unreserve an id.
In this implementation it performs essentially nothing.

-}

free :: i -> T i ()
free _ = return ()
