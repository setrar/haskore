> module Haskore.General.LoopTreeRecursive where

> import qualified Haskore.General.LoopTreeTagged as LTT
> import qualified Haskore.General.TagDictionary as Dict

> import Control.Monad.Trans.State(StateT, evalState, put, get, )
> import Control.Monad (liftM, )

Loop now needs an ID because there may be more than one of them.

> data T a =
>      Branch a [T a]
>    | Recurse (Fix (T a)) -- function with a fix-point
>    | Loop Tag            -- tag needed for resolving Recurse by 'unwind'
>
> type Fix a = a -> a
> type Tag   = Int

> example0 :: T Char
> example0 = Recurse (\x -> Branch 'a' [Recurse (\y -> Branch 'b' [y]), x])

> example1 :: T Char
> example1 =
>    Branch 'a'
>       [Recurse (\x -> Branch 'b' [x]),
>        Recurse (\y -> Branch 'c' [y])]

Implement two interleaved recursions.

  let x = f y
      y = g x z
      z = h y
  in  z

> exampleLeapFrog :: T Char
> exampleLeapFrog =
>    Recurse (\z -> Branch 'h' [
>       Recurse (\y -> Branch 'g' [
>          Branch 'f' [y],z])])

This data structure is very safe to use,
that is, it is not possible to loop to undefined tags
as in \code{LoopTreeTagged}.
But some operations are easier to perform on the tagged variant.
Especially we can not inspect the structure
of the \code{Recurse} function immediately.
Instead we have to place a \code{Loop} marker
inside the tree produced by the \code{Recurse} function.
In order to turn such a marked tree back into a \code{Recurse} function
we have to maintain a dictionary.
This is obviously not very efficient.
Intensive operations should be applied to the tagged tree.
We provide the conversions now.

The function \function{toTagged} uses duplicate tags in different branches.
They do not cause confusion but reduce data dependencies.

> toTagged :: Tag -> T a -> LTT.T Tag a
> toTagged n branch =
>    case branch of
>       Branch x s    ->  LTT.Branch x (map (toTagged n) s)
>       Recurse fe  ->  LTT.Tag n (toTagged (succ n) (fe (Loop n)))
>       Loop m      ->  LTT.Loop m

The function \function{toTaggedUnique}
employs a State in order to assign tags
that are unique overall the whole tree.

> toTaggedUnique :: Tag -> T a -> LTT.T Tag a
> toTaggedUnique n branch = evalState (toTaggedState branch) n

> toTaggedState :: (Enum tag, Monad m) => T a -> StateT tag m (LTT.T tag a)
> toTaggedState branch =
>    case branch of
>       Branch x s    ->  liftM (LTT.Branch x) (mapM toTaggedState s)
>       Recurse fe  ->  do n <- get
>                          put (succ n)
>                          liftM (LTT.Tag n)
>                                (toTaggedState (fe (Loop (fromEnum n))))
>       Loop m      ->  return (LTT.Loop (toEnum m))

> fromTagged :: (Ord tag) => LTT.T tag a -> T a
> fromTagged =
>    let conv tags branch =
>           case branch of
>              LTT.Branch x s   ->  Branch x (map (conv tags) s)
>              LTT.Tag tag x  ->  Recurse (\y -> conv
>                                           (Dict.insert tag y tags) x)
>              LTT.Loop tag   ->  Dict.lookup tags tag
>    in  conv Dict.empty

To check equality of and show Trees,
we need to supply unique Tags to each recursive loop,
which we do via a simple counter.

> instance Eq a => Eq (T a) where
>   x == y  =  toTagged 0 x == toTagged 0 y
>
> instance Show a => Show (T a) where
>   show  =  show . toTaggedUnique 0
>
> instance Functor T where
>   fmap f  =  fromTagged . fmap f . toTagged 0

Unwinding (i.e. computing fixpoints):

> unwind :: T a -> T a
> unwind (Branch x s)    = Branch x (map unwind s)
> unwind (Recurse fe)  = x where x = unwind (fe x)
> unwind (Loop _)      = error "unwind: no loop allowed in a tree"

The 2nd equation above is analogous to:
fix f = x where x = f x
And these two equations could also be written as:
fix f = f (fix f)
unwind (Rec fe) = unwind (fe (Rec fe))
