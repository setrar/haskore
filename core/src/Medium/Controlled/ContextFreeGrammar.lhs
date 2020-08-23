
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

> module Medium.Controlled.ContextFreeGrammar
>    (T, Tag(..), TagMedium, fromMedium, toMedium) where

> import qualified Medium.Controlled.List as CtrlMediumList
> import qualified Medium.Controlled      as CtrlMedium
> import Medium.Plain.ContextFreeGrammar
>    (Tag(..), joinTag, replaceInfix,
>     whileM, smallestCycle, maximumCommonInfixMulti)
> import Medium (prim, serial1, parallel1)

> import Data.Maybe (fromJust)
> import qualified Haskore.General.Map as Map

> import Control.Monad.Trans.State (state, execState)

\end{haskelllisting}

Condense all common infixes down to length 'thres'.
The infixes are replaced by some marks using the constructor Left.
They can be considered as macros or
as non-terminals in a grammar.
The normal primitives are preserved with constructor Right.
We end up with a context-free grammar of the media.

\begin{haskelllisting}

> type TagMedium key control prim = CtrlMediumList.T control (Tag key prim)

> type T key control prim = [(key, TagMedium key control prim)]

> fromMedium :: (Ord key, Ord control, Ord prim) =>
>    [key] -> Int -> CtrlMediumList.T control prim -> T key control prim
> fromMedium (key:keys) thres m =
>    let action = whileM (>= thres) (map (state . condense) keys)
>        -- action = sequence (take 1 (map (state . condense) keys))
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
>    T key control prim -> CtrlMediumList.T control prim
> toMedium = snd . head . expand

> expand :: (Show key, Ord key, Ord prim) =>
>    T key control prim -> [(key, CtrlMediumList.T control prim)]
> expand grammar =
>    let notFound key = error ("The non-terminal '" ++ show key ++ "' is unknown.")
>        dict  = Map.fromList grammar
>        dict' = Map.map (CtrlMedium.foldList expandSub serial1 parallel1
>                            CtrlMedium.control) dict
>        expandSub (Prim p) = prim p
>        expandSub (Call key) =
>           Map.findWithDefault dict' (notFound key) key
>        expandSub (CallMulti n key) =
>           serial1 (replicate n (Map.findWithDefault dict' (notFound key) key))
>    in  map (fromJust . Map.lookup (Map.mapWithKey (,) dict') . fst) grammar

\end{haskelllisting}

Find the longest common infix over all parts of the music
and replace it in all of them.

\begin{haskelllisting}

> condense :: (Ord key, Ord control, Ord prim) =>
>       key
>    -> T key control prim
>    -> (Int, T key control prim)
> condense key x =
>    let getSerials = CtrlMedium.switchList
>           (const [])
>           (\xs -> xs : concatMap getSerials xs)
>           (\xs ->      concatMap getSerials xs)
>           (const getSerials)
>        infx = smallestCycle (maximumCommonInfixMulti length
>                   (concatMap (getSerials . snd) x))
>        absorbSingleton _ [m] = m
>        absorbSingleton collect ms = collect ms
>        replaceRec = CtrlMedium.foldList prim
>           (absorbSingleton serial1 . map joinTag . replaceInfix key infx)
>           (absorbSingleton parallel1)
>           (CtrlMedium.control)
>    in  (length infx, (key, serial1 infx) : map (\(k, ms) -> (k, replaceRec ms)) x)

\end{haskelllisting}
