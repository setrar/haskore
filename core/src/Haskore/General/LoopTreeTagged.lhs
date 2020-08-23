> module Haskore.General.LoopTreeTagged where

> import qualified Haskore.General.TagDictionary as Dict

> data T tag a =
>      Branch a [T tag a]
>    | Tag tag (T tag a) -- mark a point where we want return to later
>    | Loop tag          -- return to a marked point
>        deriving (Eq, Show)

The tag for \code{Tag} must be unique,
but multiple use in \code{Loop} is allowed.
Vice versa tags for \code{Loop} must be defined by a \code{Tag} constructor.

> example0 :: T Int Char
> example0 = Tag 0 (Branch 'a' [Tag 1 (Branch 'b' [Loop 1]), Loop 0])

\begin{comment}

Eq and Show

instance (Eq tag, Eq a) => Eq (T tag a) where
  Branch x xSub   == Branch y ySub    =  x == y && xSub == ySub
  Tag xTag xSub == Tag yTag ySub  =  xTag == yTag && xSub == ySub
  Loop xTag     == Loop yTag      =  xTag == yTag
  _             == _              =  False

instance (Show tag, Show a) => Show (T tag a) where
  show (Const x)   =  "(Const " ++ show x  ++ ")"
  show (Add e1 e2) =  "(Add "   ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Tag i e)   =  "(Tag "   ++ show i  ++ " " ++ show e  ++ ")"
  show (Loop i)    =  "(Loop "  ++ show i  ++ ")"

\end{comment}

MapE:

> mapE :: (a -> b) -> T tag a -> T tag b
> mapE f =
>    let aux branch =
>           case branch of
>              Branch x sub  -> Branch (f x) (map aux sub)
>              Tag tag sub -> Tag tag (aux sub)
>              Loop tag    -> Loop tag
>    in  aux

> instance Functor (T tag) where
>    fmap = mapE

Replace all loops by the corresponding super-trees.
Internally the compiler should translate this into loops, again,
but this cannot be observed by the Haskell code anymore.

> unwind :: (Ord tag) => T tag a -> T tag a
> unwind =
>    let aux tags branch =
>           case branch of
>              Branch x sub  -> Branch x (map (aux tags) sub)
>              Tag tag sub -> let e' = aux (Dict.insert tag e' tags) sub
>                             in  e'
>              Loop tag    -> Dict.lookup tags tag
>    in  aux Dict.empty
