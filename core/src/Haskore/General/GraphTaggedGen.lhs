> module Haskore.General.GraphTaggedGen where

> import qualified Haskore.General.TagDictionary as Dict

This is a generalization from \module{Haskore.General.LoopTreeTaggedGen}
to general graphs.
The addition to that module is ``sharing''.
It doesn't seem to be worthwile to put everything into a tree based structure.
Instead we maintain a dictionary of sharing branches,
where we split the signal either for feedback or for forward sharing.

The dictionary structure should be shared
with \module{Haskore.General.LoopTreeTagged}.

> type T tag coll = Dict.T tag (Tree tag coll)
> data Tree tag coll =
>      Branch (coll (Tree tag coll))
>    | Reference tag    {- continue at one root of the dictionary,
>                          this can mean feedback or sharing -}
> --       deriving (Eq, Show)

Cf. \module{Haskore.General.LoopTreeTaggedGen}.

> class CollEq coll where
>   collEqual :: Eq tag => coll (Tree tag coll) -> coll (Tree tag coll) -> Bool

> class CollShow coll where
>   collShowsPrec :: Show tag => Int -> coll (Tree tag coll) -> ShowS

> instance (Eq tag, CollEq coll) => Eq (Tree tag coll) where
>   Branch x0    == Branch x1     =  collEqual x0 x1
>   Reference i0 == Reference i1  =  i0 == i1
>   _            == _             =  False

> instance (Show tag, CollShow coll) => Show (Tree tag coll) where
>   showsPrec p branch  =  showParen (p>10)
>     (case branch of
>        Branch x     ->  showString "Branch " . collShowsPrec 11 x
>        Reference i  ->  showString "Reference " . showsPrec 11 i)

> unwind :: (Ord tag, Functor coll) => T tag coll -> T tag coll
> unwind dict =
>    let aux branch =
>           case branch of
>              Branch x      -> Branch (fmap aux x)
>              Reference tag -> Dict.lookup newDict tag
>        newDict = fmap aux dict
>    in  newDict
