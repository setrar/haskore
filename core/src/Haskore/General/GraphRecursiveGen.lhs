> module Haskore.General.GraphRecursiveGen where

> import qualified Haskore.General.GraphTaggedGen as GTG
> import qualified Haskore.General.TagDictionary as Dict
> import Data.Traversable(Traversable)
> import qualified Data.Traversable as Traversable

> import Control.Monad.Trans.RWS (RWS, evalRWS, put, get, tell, )
> import Control.Monad (liftM, )

This is a generalization of \module{Haskore.General.LoopTreeTaggedGen}.
It adds a constructor for sharing interim results.

> data T coll =
>      Branch (coll (T coll))
>    | Recurse (Fix (T coll)) -- function with a fix-point
>    | Share (T coll) (T coll -> T coll)
>                             -- share a sub-expression among deeper sub-expressions
>    | Reference Tag          -- tag needed for resolving Recurse and Share by 'unwind'
>
> type Fix a = a -> a
> type Tag   = Int

> recourse :: Fix (T coll) -> T coll
> recourse = Recurse

> share :: (T coll) -> (T coll -> T coll) -> T coll
> share = Share

Implement this one

  let x = f y
      y = g x
  in  h x y

with recursion, but without sharing:

  h (recourse (f . g)) (recourse (g . f))

with recursion of tuples:

  uncurry h $ recourse (\(x,y) -> (f y, g x))

with recursion and sharing:

  share (f y) (\x -> share (g x) (\y -> h x y))  -- wrong!


> toTaggedUnique :: (Traversable coll) => Tag -> T coll -> GTG.T Tag coll
> toTaggedUnique n branch = snd $ evalRWS (toTaggedState branch) () n

> toTaggedState :: (Traversable coll) =>
>    T coll -> RWS () (GTG.T Tag coll) Tag (GTG.Tree Tag coll)
> toTaggedState branch =
>    case branch of
>       Branch x    ->  liftM GTG.Branch (Traversable.mapM toTaggedState x)
>       Recurse fe  ->  do t <- get
>                          put (succ t)
>                          tree <- toTaggedState (fe (Reference t))
>                          tell (Dict.singleton t tree)
>                          return tree
>       Share x fe  ->  do t <- get
>                          put (succ t)
>                          sharedTree <- toTaggedState x
>                          tell (Dict.singleton t sharedTree)
>                          toTaggedState (fe (Reference t))
>       Reference t ->  return (GTG.Reference t)

> {-
> fromTagged :: (Eq tag, Functor coll) => GTG.T tag coll -> [T coll]
> fromTagged =
>    let aux branch =
>           case branch of
>              Branch x      -> Branch (fmap aux x)
>              Reference tag -> fromMaybe
>                                  (error ("unknown reference tag"))
>                                  (lookup tag newDict)
>        newDict = map (\(tag, tree) -> (tag, aux tree)) dict
>    in  newDict

>    let conv tags branch =
>           case branch of
>              GTG.Branch x   ->  Branch (fmap (conv tags) x)
>              GTG.Tag tag x  ->  Recurse (\y -> conv
>                                            (LTT.addUnique (tag,y) tags) x)
>              GTG.Loop tag   ->  fromMaybe (error ("unknown loop tag"))
>                                    (lookup tag tags)
>    in  conv []
> -}

> instance (Traversable coll, GTG.CollEq coll) => Eq (T coll) where
>   x == y  =  toTaggedUnique 0 x == toTaggedUnique 0 y
>
> instance (Traversable coll, GTG.CollShow coll) => Show (T coll) where
>   showsPrec p x  =  showString "fromTagged " .
>                     showParen (p>10) (showsPrec 11 (toTaggedUnique 0 x))

Unwinding, i.e. computing fixpoints:

> unwind :: (Functor coll) => T coll -> T coll
> unwind (Branch x)    = Branch (fmap unwind x)
> unwind (Recurse fe)  = x where x = unwind (fe x)
> unwind (Reference _) = error "unwind: no loop allowed in a tree"
> unwind (Share x fe)  = fe (unwind x)
