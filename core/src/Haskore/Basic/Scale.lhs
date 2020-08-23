% from AutoTrack by Stefan Ratschan

\subsubsection{Scales}

\begin{haskelllisting}

> module Haskore.Basic.Scale
>          (T, ionian, dorian, phrygian, lydian, mixolydian,
>              aeolian, lokrian, altered, htwt, wtht,
>              ionianRel, dorianRel, phrygianRel, lydianRel, mixolydianRel,
>              aeolianRel, lokrianRel, alteredRel, htwtRel, wthtRel,
>
>              fromOffsets, fromIntervals, continue) where

> import qualified Haskore.Basic.Pitch as Pitch
> import Control.Monad(liftM2)

\end{haskelllisting}

Some of the following code is taken
from the EasyScale implementation of Martin Schwenke.

\begin{haskelllisting}

> type T = [Pitch.Absolute]
> type Intervals = [Pitch.Relative]

\end{haskelllisting}

Make a scale given a list of absolute pitches, usually starting at 0,
and a \type{Pitch.Class} representing the root note of the scale.

\begin{haskelllisting}

> fromOffsets :: [Pitch.Absolute] -> Pitch.Class -> T
> fromOffsets ns pc
>   = map (+ Pitch.classToInt pc) ns

\end{haskelllisting}

Create a scale from a list of intervals between successive notes.

\begin{haskelllisting}

> fromIntervals :: Intervals -> Pitch.Class -> T
> fromIntervals = fromOffsets . scanl (+) 0

\end{haskelllisting}

Continue a scale to all octaves.

\begin{haskelllisting}

> continue :: T -> T
> continue = liftM2 (+) (iterate (12+) 0)

\end{haskelllisting}

Now some general useful scales from music theory.

\begin{haskelllisting}

> ionianRel, dorianRel, phrygianRel, lydianRel, mixolydianRel,
>   aeolianRel, lokrianRel, alteredRel, htwtRel,
>   wthtRel :: Intervals

> ionianRel     = [ 2, 2, 1, 2, 2, 2, 1 ]
> dorianRel     = [ 2, 1, 2, 2, 2, 1, 2 ]
> phrygianRel   = [ 1, 2, 2, 2, 1, 2, 2 ]
> lydianRel     = [ 2, 2, 2, 1, 2, 2, 1 ]
> mixolydianRel = [ 2, 2, 1, 2, 2, 1, 2 ]
> aeolianRel    = [ 2, 1, 2, 2, 1, 2, 2 ]
> lokrianRel    = [ 1, 2, 2, 1, 2, 2, 2 ]
> alteredRel    = [ 1, 2, 1, 2, 2, 2, 2 ]
> htwtRel       = [ 1, 2, 1, 2, 1, 2, 1, 2 ]
> wthtRel       = [ 2, 1, 2, 1, 2, 1, 2, 1 ]

> ionian, dorian, phrygian, lydian, mixolydian,
>   aeolian, lokrian, altered, htwt,
>   wtht :: Pitch.Class -> T

> ionian     = fromIntervals ionianRel
> dorian     = fromIntervals dorianRel
> phrygian   = fromIntervals phrygianRel
> lydian     = fromIntervals lydianRel
> mixolydian = fromIntervals mixolydianRel
> aeolian    = fromIntervals aeolianRel
> lokrian    = fromIntervals lokrianRel
> altered    = fromIntervals alteredRel
> htwt       = fromIntervals htwtRel
> wtht       = fromIntervals wthtRel

\end{haskelllisting}

Example:
Alternatively to applying \function{continue} to a scale
you can create an infinitely increasing scale
using the definition by intervals,
e.g. \code{fromIntervals (cycle ionianRel) Pitch.C}.
