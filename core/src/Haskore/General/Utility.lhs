\subsection{Utility functions}

\begin{haskelllisting}

> module Haskore.General.Utility where
>
> import System.Random (randomRs, mkStdGen, )
> import Data.List.HT (segmentBefore, )
> import Data.List (foldl', )
> import Data.Ratio ((%), denominator, numerator, Ratio, )
> import qualified Haskore.General.Map as Map

\end{haskelllisting}


\function{splitBy} takes a boolean test and a list;
it divides up the list and turns it into a {\em list of sub-lists};
each sub-list consists of
\begin{enumerate}
\item
one element for which the test is true (or the first element in the list), and
\item
all elements after that element for which the test is false.
\end{enumerate}
For example, \code{splitBy (>10) [27, 0, 2, 1, 15, 3, 42, 4]}
yields \code{[ [27,0,2,1], [15,3], [42,4] ]}.
\begin{haskelllisting}

> splitBy :: (a -> Bool) -> [a] -> [[a]]
> splitBy p = dropWhile null . segmentBefore p

\end{haskelllisting}

\function{segmentBefore} will have at most one empty list at the beginning,
which is dropped by \function{dropWhile}.

It should have signature
  segmentBefore :: (a -> Bool) -> [a] -> ([a], [(a, [a])])
or even better
  segmentBefore :: (a -> Bool) -> [a] -> AlternatingListUniform.T a [a]
and could be implemented using Uniform.fromEitherList

A variant of \function{foldr} and \function{foldr1}
which works only for non-empty lists
and initializes the accumulator depending on the last element of the list.

\begin{haskelllisting}

> foldrf :: (a -> b -> b) -> (a -> b) -> [a] -> b
> foldrf f g =
>    let aux []     = error "foldrf: list must be non-empty"
>        aux (x:[]) = g x
>        aux (x:xs) = f x (aux xs)
>    in  aux

\end{haskelllisting}



\function{flattenTuples2} flattens a list of pairs into a list.
Similarly, \function{flattenTuples3} flattens a list of 3-tuples into a list,
and so on.
\begin{haskelllisting}

> flattenTuples2 :: [(a,a)]     -> [a]
> flattenTuples3 :: [(a,a,a)]   -> [a]
> flattenTuples4 :: [(a,a,a,a)] -> [a]
>
> flattenTuples2 = concatMap (\(x,y)     -> [x,y])
> flattenTuples3 = concatMap (\(x,y,z)   -> [x,y,z])
> flattenTuples4 = concatMap (\(x,y,z,w) -> [x,y,z,w])

\end{haskelllisting}


Variants of \function{zip} and \function{zip3}
which check that all argument lists have the same length.

\begin{haskelllisting}

> zipWithMatch :: (a -> b -> c) -> [a] -> [b] -> [c]
> zipWithMatch f (x:xs) (y:ys) = f x y : zipWithMatch f xs ys
> zipWithMatch _ [] [] = []
> zipWithMatch _ _ _ = error "zipWithMatch: lengths of lists differ"

> zipWithMatch3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
> zipWithMatch3 f (x:xs) (y:ys) (z:zs) = f x y z : zipWithMatch3 f xs ys zs
> zipWithMatch3 _ [] [] [] = []
> zipWithMatch3 _ _ _ _ = error "zipWithMatch3: lengths of lists differ"

\end{haskelllisting}

This is a variant of \function{maximum}
which returns at least zero, i.e. always a non-negative number.
This is necessary for determining the length of a parallel music composition
where the empty list has zero duration.

\begin{haskelllisting}

> maximum0 :: (Ord a, Num a) => [a] -> a
> maximum0 = foldl' max 0

\end{haskelllisting}


Convert a mapping (i.e. list of pairs) to a function, and use this for a
translation function, which translates every character in a by replacing it by
looking it up in l2 and replacing it with the according character in l2.

\begin{haskelllisting}

> translate :: (Ord a) => [ a ] -> [ a ] -> [ a ] -> [ a ]
> translate l1 l2 a =
>    if length l1 == length l2
>    then let table = Map.fromList (zip l1 l2)
>         in  map (\x -> Map.findWithDefault table x x) a
>    else error "translate: lists must have equal lengths"

\end{haskelllisting}

A random list of integers between 0 and n.

\begin{haskelllisting}

> randList :: Int -> [ Int ]
> randList n = randomRs (0, n) (mkStdGen 0)

\end{haskelllisting}

Is one rational divisible by another one (i.e., is it a integer multiple of it)?

\begin{haskelllisting}

> divisible :: Integral a => Ratio a -> Ratio a -> Bool
> divisible r1 r2 =
>    0 == mod (numerator r1 * denominator r2)
>             (numerator r2 * denominator r1)

\end{haskelllisting}

Do the division.

\begin{haskelllisting}

> divide :: Integral a => Ratio a -> Ratio a -> a
> divide r1 r2 =
>    let (q, r) = divideModulus r1 r2
>    in  if r == 0
>        then q
>        else error "Utility.divide: rationals are indivisible"

> modulus :: Integral a => Ratio a -> Ratio a -> Ratio a
> modulus r1 r2 = snd (divideModulus r1 r2)

> divideModulus :: Integral a => Ratio a -> Ratio a -> (a, Ratio a)
> divideModulus r1 r2 =
>    let (q, r) = divMod (numerator r1 * denominator r2)
>                        (numerator r2 * denominator r1)
>    in  (q, r % (denominator r1 * denominator r2))

\end{haskelllisting}

Also the GCD can be generalized to ratios:

\begin{haskelllisting}

> gcdDur :: Integral a => Ratio a -> Ratio a -> Ratio a
> gcdDur x1 x2 =
>    let a = numerator x1
>        b = denominator x1
>        c = numerator x2
>        d = denominator x2
>    in  gcd a c % lcm b d

\end{haskelllisting}
