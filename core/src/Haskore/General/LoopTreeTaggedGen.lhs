> module Haskore.General.LoopTreeTaggedGen where

> import qualified Haskore.General.TagDictionary as Dict


Similar to \module{Haskore.General.LoopTreeTagged},
but here the sub-trees are organized in general collection types \type{coll}.
Actually we do not want to use generic collections, like Set or so,
but we want to store custom data plus sub-trees in \type{coll} type objects.

> data T tag coll =
>      Branch (coll (T tag coll))
>    | Tag tag (T tag coll)  -- mark a point where we want return to later
>    | Loop tag              -- return to a marked point
> --       deriving (Eq, Show)

In order to avoid non-standard instance class contexts,
undecidable instances and other mess,
we define the classes CollEq and CollShow,
which allow implementation of Eq and Show instances
for collections without making assumptions about the collection members.
Coding CollEq and CollShow instances for collections is quite boring
because this is mainly replication of code
that would be otherwise generated automatically due to a 'deriving' clause.

(Proposed by Roberto Zunino <roberto.zunino@sns.it>
2006-03-11 in haskell-cafe@haskell.org)

> class CollEq coll where
>   collEqual :: Eq tag => coll (T tag coll) -> coll (T tag coll) -> Bool

> class CollShow coll where
>   collShowsPrec :: Show tag => Int -> coll (T tag coll) -> ShowS

> instance (Eq tag, CollEq coll) => Eq (T tag coll) where
>   Branch x0     == Branch x1      =  collEqual x0 x1
>   Tag tag0 x0 == Tag tag1 x1  =  tag0 == tag1 && x0 == x1
>   Loop i0     == Loop i1      =  i0 == i1
>   _           == _            =  False

> instance (Show tag, CollShow coll) => Show (T tag coll) where
>   showsPrec p branch  =  showParen (p>10)
>     (case branch of
>        Branch x   ->  showString "Branch " . collShowsPrec 11 x
>        Tag i e  ->  showString "Tag "  . showsPrec 11 i
>                       . showString " " . showsPrec 11 e
>        Loop i   ->  showString "Loop " . showsPrec 11 i)

> unwind :: (Ord tag, Functor coll) => T tag coll -> T tag coll
> unwind =
>    let aux tags branch =
>           case branch of
>              Branch x      -> Branch (fmap (aux tags) x)
>              Tag tag sub -> let e' = aux (Dict.insert tag e' tags) sub
>                             in  e'
>              Loop tag    -> Dict.lookup tags tag
>    in  aux Dict.empty
