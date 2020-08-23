
\subsection{Structure Analysis}

This module contains a function which builds
a hierarchical music object from a serial one.
This is achieved by searching for long common infixes.
A common infix is replaced by a single object
at each occurence.

This module proofs the sophistication of the separation
between general arrangement of some objects as provided by the \module{Medium}
and the special needs of music provided by the \module{Music}.
It's possible to formulate these algorithms without the knowledge of Music
and we can insert the type \code{Tag} to distinguish
between media primitives and macro calls.
The only drawback is that it is not possible to descend
into controlled sub-structures, like Tempo and Trans.

\begin{haskelllisting}

> module Medium.Plain.ContextFreeGrammar where

> import Data.List.HT (tails, mapAdjacent, )
> import Data.List (sort, isPrefixOf, findIndex, )
> import Data.Maybe (fromJust, )
> import qualified Haskore.General.Map as Map
> import qualified Data.List.Key as Key

> import Control.Monad.Trans.State (StateT, put, get, state, execState, )

> import Medium (prim, serial1, parallel1)
> import qualified Medium
> import qualified Medium.Plain.List as ListMedium

\end{haskelllisting}

Condense all common infixes down to length 'thres'.
The infixes are replaced by some marks using the constructor Left.
They can be considered as macros or
as non-terminals in a grammar.
The normal primitives are preserved with constructor Right.
We end up with a context-free grammar of the media.

\begin{haskelllisting}

> data Tag key prim =
>      Prim prim
>    | Call key
>    | CallMulti Int key
>    deriving (Eq, Ord, Show)
> type TagMedium key prim = ListMedium.T (Tag key prim)

> -- True is for cyclic infixes
> type T key prim = [(key, TagMedium key prim)]

> fromMedium :: (Ord key, Ord prim) =>
>    [key] -> Int -> ListMedium.T prim -> T key prim
> fromMedium (key:keys) thres m =
>    let action = whileM (>= thres) (map (state . condense) keys)
>        -- action = sequence (take 1 (map (State . condense) keys))
>    in  reverse $ execState action [(key, fmap Prim m)]
> fromMedium _ _ _ =
>    error ("No key given."++
>       " Please provide an infinite or at least huge number of macro names.")

\end{haskelllisting}

The inverse of \code{fromMedium}: Expand all macros.
Cyclic macro references shouldn't be a problem
if it is possible to resolve the dependencies.
We manage the grammar in the dictionary \code{dict}.
Now a naive way for expanding the macros
is to recourse into each macro call manually
using lookups to \code{dict}.
This would imply that we need new memory for each expansion of the same macro.
We have chosen a different approach:
We map \code{dict} to a new dictionary \code{dict'}
which contains the expanded versions of each Medium.
For expansion we don't use repeated lookups to \code{dict}
but we use only one lookup to \code{dict'}
-- which contains the fully expanded version of the considered Medium.
This method is rather the same as
if you write Haskell values that invokes each other.

The function \code{expand} computes the expansion for each key and
the function \code{toMedium} computes the expansion of the first macro.
Thus \code{toMedium} quite inverts \code{fromMedium}.

\begin{haskelllisting}

> toMedium :: (Show key, Ord key, Ord prim) =>
>    T key prim -> ListMedium.T prim
> toMedium = snd . head . expand

> expand :: (Show key, Ord key, Ord prim) =>
>    T key prim -> [(key, ListMedium.T prim)]
> expand grammar =
>    let notFound key = error ("The non-terminal '" ++ show key ++ "' is unknown.")
>        dict  = Map.fromList grammar
>        dict' = Map.map (Medium.foldList expandSub serial1 parallel1) dict
>        expandSub (Prim p) = prim p
>        expandSub (Call key) =
>           Map.findWithDefault dict' (notFound key) key
>        expandSub (CallMulti n key) =
>           serial1 (replicate n (Map.findWithDefault dict' (notFound key) key))
>    in  map (fromJust . Map.lookup (Map.mapWithKey (,) dict') . fst) grammar

\end{haskelllisting}


Do monadic actions until the condition \code{p} fails.
This is implemented for State Monads,
because in plain Monads one could not reset the state
and thus the state wouldn't be that after
the last successful (with respect to the predicate \code{p}) action.

\begin{haskelllisting}

> whileM :: (Monad m) => (a -> Bool) -> [StateT s m a] -> StateT s m [a]
> whileM _ [] = return []
> whileM p (m:ms) =
>    do s <- get
>       x <- m
>       if p x then whileM p ms >>= return . (x:)
>              else put s -- reset to the old state
>                     >> return []

\end{haskelllisting}

Find the longest common infix over all parts of the music
and replace it in all of them.

\begin{haskelllisting}

> condense :: (Ord key, Ord prim) =>
>       key
>    -> T key prim
>    -> (Int, T key prim)
> condense key x =
>    let getSerials = Medium.switchList
>           (const [])
>           (\xs -> xs : concatMap getSerials xs)
>           (\xs ->      concatMap getSerials xs)
>        infx = smallestCycle (maximumCommonInfixMulti length
>                   (concatMap (getSerials . snd) x))
>        absorbSingleton _ [m] = m
>        absorbSingleton collect ms = collect ms
>        replaceRec = Medium.foldList prim
>           (absorbSingleton serial1 . map joinTag . replaceInfix key infx)
>           (absorbSingleton parallel1)
>    in  (length infx, (key, serial1 infx) : map (\(k, ms) -> (k, replaceRec ms)) x)

> joinTag :: Medium.Construct medium =>
>    Tag key (medium (Tag key prim)) -> medium (Tag key prim)
> joinTag (Prim m)        = m
> joinTag (Call k)        = prim (Call k)
> joinTag (CallMulti n k) = prim (CallMulti n k)

\end{haskelllisting}

Replace all occurences of the infix by its key.
Collect accumulated occurences in one \code{CallMulti}.

\begin{haskelllisting}

> replaceInfix :: (Eq a, Eq b) =>
>       a
>    -> [b]
>    -> [b]
>    -> [Tag a b]
> replaceInfix key infx sequ =
>    let recourse [] = []
>        recourse xa@(x:xs) =
>           let pref = commonPrefix (cycle infx) xa
>               (num, r) = divMod (length pref) (length infx)
>               len = length pref - r
>           in  if num == 0
>               then Prim x : recourse xs
>               else ((if num == 1 then Call key else CallMulti num key)
>                       : recourse (drop len xa))
>    in  recourse sequ

\end{haskelllisting}

A common infix indicates a loop if its occurences overlap.
We can detect this by checking if there is a suffix of our list
which is also a prefix of this list.

\begin{haskelllisting}

> isCyclic :: Eq a => [a] -> Bool
> isCyclic x = any (flip isPrefixOf x) (init (tail (tails x)))

\end{haskelllisting}

Find the shortest list \code{y},
where \code{x} is a prefix of \code{cycle y}.
If \code{x} has no loop, then \code{x == y}.

\begin{haskelllisting}

> smallestCycle :: Eq a => [a] -> [a]
> smallestCycle x =
>    take (1 + fromJust (findIndex (flip isPrefixOf x) (tail (tails x)))) x

\end{haskelllisting}

Finding common infixes is a prominent application of suffix trees.
But since I don't have an implementation of suffix trees
I'll stick to a sorted list of suffices.

\begin{haskelllisting}

> maximumCommonInfix :: (Ord a, Ord b) => ([a] -> b) -> [a] -> [a]
> maximumCommonInfix mag =
>    Key.maximum mag .
>    mapAdjacent commonPrefix .
>    sort . tails

\end{haskelllisting}

Find common infixes across multiple strings.
This could be a nice application of generalized suffix trees.

\begin{haskelllisting}

> maximumCommonInfixMulti :: (Ord a, Ord b) => ([a] -> b) -> [[a]] -> [a]
> maximumCommonInfixMulti mag =
>    Key.maximum mag .
>    mapAdjacent commonPrefix .
>    sort . concatMap tails

\end{haskelllisting}

Find the longest common prefix.
(Two implementations that may be used for testing.)

\begin{haskelllisting}

> commonPrefix :: Eq a => [a] -> [a] -> [a]
> commonPrefix xs ys =
>    map fst $ takeWhile (uncurry (==)) $ zip xs ys

> commonPrefixRec :: Eq a => [a] -> [a] -> [a]
> commonPrefixRec (x:xs) (y:ys) =
>    if x == y
>      then x : commonPrefix xs ys
>      else []
> commonPrefixRec _ _ = []

\end{haskelllisting}
