> module Haskore.General.LoopTreeRecursiveGen where

> import qualified Haskore.General.LoopTreeTaggedGen as LTTG
> import qualified Haskore.General.TagDictionary as Dict

> import Data.Traversable(Traversable)
> import qualified Data.Traversable as Traversable

> import Control.Monad.Trans.State(StateT, evalState, put, get)
> import Control.Monad (liftM, )

The Loop constructor should not be used by users.
It is only necessary for interim results of 'toTagged'.
With the type \code{data ListTree a b = ListTree a [b]},
a \type{LoopTreeRecursiveGen.T (ListTree a)}
is isomoprhic to \type{LoopTreeRecursive.T a}.
'Tag' is a fixed type instead of a type variable
since it is only needed for internal issues.

> data T coll =
>      Branch (coll (T coll))
>    | Recurse (Fix (T coll)) -- function with a fix-point
>    | Loop Tag               -- tag needed for resolving Recurse by 'unwind'
>
> type Fix a = a -> a
> type Tag   = Int

> recourse :: Fix (T coll) -> T coll
> recourse = Recurse

> toTagged :: (Functor coll) => Tag -> T coll -> LTTG.T Tag coll
> toTagged n branch =
>    case branch of
>       Branch x      ->  LTTG.Branch (fmap (toTagged n) x)
>       Recurse fe  ->  LTTG.Tag n (toTagged (succ n) (fe (Loop n)))
>       Loop m      ->  LTTG.Loop m

> toTaggedUnique :: (Traversable coll) => Tag -> T coll -> LTTG.T Tag coll
> toTaggedUnique n branch = evalState (toTaggedState branch) n

> toTaggedState :: (Traversable coll, Enum tag, Monad m) =>
>    T coll -> StateT tag m (LTTG.T tag coll)
> toTaggedState branch =
>    case branch of
>       Branch x      ->  liftM LTTG.Branch (Traversable.mapM toTaggedState x)
>       Recurse fe  ->  do n <- get
>                          put (succ n)
>                          liftM (LTTG.Tag n)
>                                (toTaggedState (fe (Loop $ fromEnum n)))
>       Loop m      ->  return (LTTG.Loop $ toEnum m)

> fromTagged :: (Functor coll) => LTTG.T Tag coll -> T coll
> fromTagged =
>    let conv tags branch =
>           case branch of
>              LTTG.Branch x     ->  Branch (fmap (conv tags) x)
>              LTTG.Tag tag x  ->  Recurse (\y -> conv
>                                            (Dict.insert tag y tags) x)
>              LTTG.Loop tag   ->  Dict.lookup tags tag
>    in  conv Dict.empty


> instance (Functor coll, LTTG.CollEq coll) => Eq (T coll) where
>   x == y  =  toTagged 0 x == toTagged 0 y
>
> instance (Functor coll, LTTG.CollShow coll) => Show (T coll) where
>   showsPrec p x  =  showString "fromTagged " .
>                     showParen (p>10) (showsPrec 11 (toTagged 0 x))

Unwinding, i.e. computing fixpoints:

> unwind :: (Functor coll) => T coll -> T coll
> unwind (Branch x)      = Branch (fmap unwind x)
> unwind (Recurse fe)  = x where x = unwind (fe x)
> unwind (Loop _)      = error "unwind: no loop allowed in a tree"
