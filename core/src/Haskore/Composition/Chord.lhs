\subsubsection{Chords}
\seclabel{chords}

Earlier I described how to represent chords as values of type \code{Music.T}.
However, sometimes it is convenient to treat chords more abstractly.
Rather than think of a chord in terms of its actual notes,
it is useful to think of it in terms of its chord ``quality'',
coupled with the key it is played in and the particular voicing used.
For example, we can describe a chord as being a ``major triad in root
position, with root middle C''.  Several approaches have been put
forth for representing this information, and we cannot cover all of
them here.  Rather, I will describe two basic representations, leaving
other alternatives to the skill and imagination of the
reader.\footnote{For example, Forte prescribes normal forms for chords
in an atonal setting \cite{forte}.}

First, one could use a \keyword{pitch} representation, where each note is
represented as its distance from some fixed pitch.  \code{0} is the
obvious fixed pitch to use, and thus, for example, \code{[0,4,7]}
represents a major triad in root position.  The first zero is in some
sense redundant, of course, but it serves to remind us that the chord
is in ``normal form''.  For example, when forming and transforming
chords, we may end up with a representation such as \code{[2,6,9]},
which is not normalized; its normal form is in fact \code{[0,4,7]}.
Thus we define:
\begin{quote}
A chord is in \keyword{pitch normal form} if the first pitch is zero,
and the subsequent pitches are monotonically increasing.
\end{quote}

One could also represent a chord \keyword{intervalically}; i.e.~as a
sequence of intervals.  A major triad in root position, for example,
would be represented as \code{[4,3,-7]}, where the last interval
``returns'' us to the ``origin''. Like the \code{0} in the pitch
representation, the last interval is redundant, but allows us to
define another sense of normal form:
\begin{quote}
A chord is in \keyword{interval normal form} if the intervals are all
greater than zero, except for the last which must be equal to the
negation of the sum of the others.
\end{quote}
In either case, we can define a chord type as:
\begin{haskelllisting}

> module Haskore.Composition.Chord where
>
> import qualified Haskore.Music  as Music
> import qualified Haskore.Melody as Melody
> import qualified Haskore.Basic.Pitch    as Pitch
> import qualified Haskore.Basic.Interval as I
> import           Haskore.General.Utility (foldrf, )
> import           Data.Ord.HT (comparing, )
> import           Data.List.HT (viewR, )
> import           Data.List (genericLength, minimumBy, )
>
> type T = [Pitch.Relative]

\end{haskelllisting}


We might ask whether there is some advantage, computationally, of
using one of these representations over the other.  However, there is
an invertible linear transformation between them, as defined by the
following functions, and thus there is in fact little advantage of one
over the other:
\begin{haskelllisting}

> pitchToInterval :: T -> T
> pitchToInterval [] = error "pitchToInterval: Chord must be non-empty."
> pitchToInterval ch@(p:ps) =
>    zipWith (-) (ps++[p]) ch
>
> intervalToPitch :: T -> T
> intervalToPitch [] = error "intervalToPitch: Chord must be non-empty."
> intervalToPitch ch =
>    let Just (chInit, chLast) = viewR (scanl (+) 0 ch)
>    in  if chLast==0
>          then chInit
>          else error "intervalToPitch: intervals do not sum-up to zero."

\end{haskelllisting}

\begin{exercise}
Show that \code{pitchToInterval} and \code{intervalToPitch}
are \keyword{inverses} in the following sense:
for any chord \code{ch1} in pitch normal form, and
\code{ch2} in interval normal form, each of length at least two:
\begin{center}
\code{intervalToPitch (pitchToInterval ch1) = ch1}\\
\code{pitchToInterval (intervalToPitch ch2) = ch2}
\end{center}
\end{exercise}

Another operation we may wish to perform is a test for \keyword{equality}
on chords, which can be done at many levels: based only on chord
quality, taking inversion into account, absolute equality, etc.  Since
the above normal forms guarantee a unique representation, equality of
chords with respect to chord quality and inversion is simple: it is
just the standard (overloaded) equality operator on lists.  On the
other hand, to measure equality based on chord quality alone, we need
to account for the notion of an \keyword{inversion}.

Using the pitch representation, the inversion of a chord can be
defined as follows:
\begin{haskelllisting}

> pitchInvert, intervalInvert :: T -> T
> pitchInvert (0:p2:ps) = 0 : map (subtract p2) ps ++ [12-p2]
> pitchInvert _ =
>    error "pitchInvert: Pitch chord representation must start with a zero."

\end{haskelllisting}
Although we could also directly define a function to invert
a chord given in interval representation, we will simply
define it in terms of functions already defined:
\begin{haskelllisting}

> intervalInvert = pitchToInterval . pitchInvert . intervalToPitch

\end{haskelllisting}
% pitchInvert [0,4,7] => [4,7,0] => [0,3,-4] => [0,3,8]
% intervalInvert [4,3,-7] => [3,-7,4] => [3,5,4] => [3,5,-8]

We can now determine whether a chord in normal form has the same
quality (but possibly different inversion) as another chord in normal
form, as follows: simply test whether one chord is equal either to the
other chord or to one of its inversions.  Since there is only a finite
number of inversions, this is well defined.  In Haskell:
\begin{haskelllisting}

> samePitch, sameInterval :: T -> T -> Bool
> samePitch ch1 ch2 =
>  let invs = take (length ch1) (iterate pitchInvert ch1)
>  in  ch2 `elem` invs
>
> sameInterval ch1 ch2 =
>  let invs = take (length ch1) (iterate intervalInvert ch1)
>  in  ch2 `elem` invs

\end{haskelllisting}
For example, \code{samePitch [0,4,7] [0,5,9]} returns \code{True}
(since \code{[0,5,9]} is the pitch normal form for the second inversion
of \code{[0,4,7]}).


Here we provide a list of some common types of chords.

%\begin{figure}
\begin{haskelllisting}

> majorInt, minorInt, majorSeventhInt, minorSeventhInt,
>    dominantSeventhInt, minorMajorSeventhInt,
>    sustainedFourthInt :: [Pitch.Relative]

> majorInt = [I.unison, I.majorThird, I.fifth]
> minorInt = [I.unison, I.minorThird, I.fifth]

> majorSeventhInt      = [I.unison, I.majorThird, I.fifth, I.majorSeventh]
> minorSeventhInt      = [I.unison, I.minorThird, I.fifth, I.minorSeventh]
> dominantSeventhInt   = [I.unison, I.majorThird, I.fifth, I.minorSeventh]
> minorMajorSeventhInt = [I.unison, I.minorThird, I.fifth, I.majorSeventh]

> sustainedFourthInt = [I.unison, I.fourth, I.fifth]

> type Inversion = Int

> fromIntervals ::
>    [Pitch.Relative] -> Inversion -> Music.T note -> [Music.T note]
> fromIntervals int inv m =
>    let err = error ("Chord.fromInterval: inversion number "
>                       ++ show inv ++ " too large")
>    in  map (flip Music.transpose m) (zipWith const
>              (drop inv (init (int ++ map (12+) int) ++ repeat err)) int)

> major, minor, majorSeventh, minorSeventh, dominantSeventh,
>    minorMajorSeventh, sustainedFourth ::
>       Inversion -> Music.T note -> [Music.T note]

> major = fromIntervals majorInt
> minor = fromIntervals minorInt

> majorSeventh      = fromIntervals majorSeventhInt
> minorSeventh      = fromIntervals minorSeventhInt
> dominantSeventh   = fromIntervals dominantSeventhInt
> minorMajorSeventh = fromIntervals minorMajorSeventhInt

> sustainedFourth = fromIntervals sustainedFourthInt

\end{haskelllisting}
%\caption{Common chords.}
%\figlabel{chords}
%\end{figure}

We want to offer a special service:
The computer shall find out inversions for chords in a sequence
such that the overall pitch does not vary so much.

A very simple approach is to compute the ``center'' of a chord,
that is the average of all pitches.
We do now try to keep the center as close as possible to an overall trend.
This is especially easy because for a chord of $n$ notes
the change to the next inversion
moves the center of the chord by $\frac{12}{n}$ tones.

The function gets the inversion of the first and the last chord and
the list of chords represented by the base note and
the intervals of all notes of the chord.
\begin{haskelllisting}

> data Generic attr = Generic {
>    genericPitchClass :: Pitch.Class,
>    genericIntervals  :: T,
>    genericDur        :: Music.Dur,
>    genericAttr       :: attr}
>
> type Boundary = (Pitch.T, Pitch.T)
>
> generic :: Pitch.Class -> T -> Music.Dur -> attr -> Generic attr
> generic = Generic
>
> leastVaryingInversions ::
>    Boundary -> [Generic attr] -> [[Melody.T attr]]
> leastVaryingInversions (begin,end) gs =
>    let beginCenter = fromIntegral (Pitch.toInt begin)
>        endCenter   = fromIntegral (Pitch.toInt end)
>        steep = (endCenter - beginCenter) / (genericLength gs - 1)
>        trend = map (\k -> beginCenter + steep * fromIntegral k)
>                    [0 .. (length gs - 1)]
>        invs = zipWith
>                  (\g t -> round (matchingInversion g t))
>                  gs trend
>    in  zipWith genericToNotes invs gs
>
> inversionIncrement :: T -> Double
> inversionIncrement ps = 12 / genericLength ps
>
> matchingInversion :: Generic attr -> Double -> Double
> matchingInversion g dst =
>   let c = chordCenter g
>       inc = inversionIncrement (genericIntervals g)
>   in  (dst-c)/inc
>
> mean :: [Pitch.Relative] -> Double
> mean ps = sum (map fromIntegral ps) / genericLength ps
>
> chordCenter :: Generic attr -> Double
> chordCenter (Generic pc ps _ _) =
>    fromIntegral (Pitch.classToInt pc) + mean ps
>
> boundaryCenter :: (Pitch.Octave,Inversion) -> Generic attr -> Double
> boundaryCenter (oct,inv) g =
>    12 * fromIntegral oct + chordCenter g +
>       fromIntegral inv * inversionIncrement (genericIntervals g)
>
> invert :: Inversion -> T -> T
> invert inv ps =
>    let (q,r) = divMod inv (length ps)
>    in  zipWith (+) ps
>           (replicate r (12*(q+1)) ++ repeat (12*q))
>
> genericToNotes :: Inversion -> Generic attr -> [Melody.T attr]
> genericToNotes inv (Generic pc ps dur attr) =
>    map (\t -> Melody.note (Pitch.transpose t (0,pc)) dur attr)
>        (invert inv ps)

\end{haskelllisting}

A more complicated algorithm will also work for other definitions of variation.
We compute the mean pitch for every chord
and minimize the variation of the pitch.
The variation is defined here
as the sum of the squared differences of successive chords.

This leads to a shortest ways search in a graph
where each inversion of a chord is a node
and each possible neighbourhood of inversions is an edge.
The nodes for the inversions of a chord
and the nodes for the inversions of the succeeding chord
build a complete bi-partite graph.

First we write a shortest ways search algorithm
that is specialised to our problem.
In each step we process one chord.
We construct a list of inversions,
where each inversion is associated
with the optimal way from the beginning chord to this inversion
and its variation.
This list passed to the processing of the next chord.
For reasons of simplicity we process the list backwards.

The inputs of the algorithm are a distance function
and the list of concurrent inversions for each chord.
The first element of the list contains all starting inversions,
the last element contains all ending inversions.
If you want a definitive start and end inversion,
use one-element lists.
The output is the list of the optimal inversion for each chord.
More precisely it is a list of all optimal ways,
where for each starting inversion there is one optimal way
to the closest ending inversion.
\begin{haskelllisting}

> shortestWays :: (Num b, Ord b) =>
>    (a -> a -> b) -> [[a]] -> [(b,[a])]
> shortestWays dist =
>    foldrf (processZone dist) (map (\x->(0,[x])))

> processZone :: (Num b, Ord b) =>
>    (a -> a -> b) -> [a] -> [(b,[a])] -> [(b,[a])]
> processZone dist srcs ways =
>    let distToWay src (d,dst:_) = d + dist src dst
>        distToWay _   (_,[])    =
>           error "processZone: list is never empty if called from shortestWays"
>    in  map (\src -> minimumBy (comparing fst)
>               (map (\way -> (distToWay src way, src : snd way)) ways)) srcs

> propShortestWays :: Int -> Int -> Bool
> propShortestWays n k =
>    let sws = shortestWays (\x y -> (x-y)^(2::Int))
>                           (replicate n [0..(n*k)] ++ [[0]])
>    in  head sws == (0, replicate (n+1) 0)  &&
>        last sws == (n*k^(2::Int), reverse [0,k..n*k])

\end{haskelllisting}

This routine could be made more efficient
because the centers of the chords with different inversions are equidistant.

\begin{haskelllisting}

> leastVaryingInversionsSW ::
>    Boundary -> [Generic attr] -> [[Melody.T attr]]
> leastVaryingInversionsSW bnd gs =
>    let dist (_,c0) (_,c1) = (c0-c1)^(2::Int)
>        [(_,invs)] =
>           shortestWays dist
>              (inversionCenters bnd gs)
>    in  zipWith (\(inv,_) -> genericToNotes inv) invs gs
>
> inversionCenters :: Boundary -> [Generic attr] -> [[(Inversion,Double)]]
> inversionCenters (begin,end) gs =
>    let margin = 7
>        beginCenter = fromIntegral (Pitch.toInt begin)
>        endCenter   = fromIntegral (Pitch.toInt end)
>        lower = min beginCenter endCenter - margin
>        upper = max beginCenter endCenter + margin
>        inversions g =
>           let c = chordCenter g
>               inc = inversionIncrement (genericIntervals g)
>               invs :: [Inversion]
>               invs = [floor ((lower-c)/inc) ..
>                       ceiling ((upper-c)/inc)]
>           in  map (\inv -> (inv, c + inc * fromIntegral inv)) invs
>        boundInv g center =
>           (round (matchingInversion g center), center)
>    in  [[boundInv (head gs) beginCenter]] ++
>        map inversions (tail (init gs)) ++
>        [[boundInv (last gs) endCenter]]

\end{haskelllisting}



Now two helper functions for creating a harmonic and a melodic chord,
that is chords of notes of the same length
in sequentially or simultaneously.

\begin{haskelllisting}

> melodicGen, harmonicGen :: attr -> Music.Dur ->
>    [Music.Dur -> attr -> Melody.T attr] -> Melody.T attr
> melodicGen  attr d = Music.line  . map (\n -> n d attr)
> harmonicGen attr d = Music.chord . map (\n -> n d attr)

\end{haskelllisting}
