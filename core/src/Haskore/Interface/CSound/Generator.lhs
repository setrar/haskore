\paragraph{Function Tables}
\seclabel{function-table}

Each function table must have a unique integer ID (\type{Table}),
creation time (usually 0), size (which must be a power of 2), and a
{\tt Normalize} flag.  Most tables in CSound are normalized, i.e.\
rescaled to a maximum absolute value of 1.  The normalization process
can be skipped by setting the {\tt Normalize} flag to {\tt False}.
Such a table may be desirable to generate a control or modifying
signal, but is not very useful for audio signal generation.

Tables are simply arrays of floating point values.  The values stored
in the table are calculated by one of CSound's predefined \keyword{generating
routines}, represented by the type {\tt Generator.T}:
\begin{haskelllisting}

> module Haskore.Interface.CSound.Generator where
>
> import Haskore.Interface.CSound (Time)
> import Haskore.General.Utility
>    (flattenTuples2, flattenTuples3, flattenTuples4)
>
> data T = Routine Number [Parameter]
>        | SoundFile SFName SkipTime ChanNum
>      deriving Show
>
> type SFName    = String
> type SkipTime  = Time
> type ChanNum   = Float
> type Number    = Int
> type Parameter = Float

\end{haskelllisting}
{\tt Routine n args} refers to CSound's generating routine $n$ (an
integer), called with floating point arguments {\tt args}.  There is
only one generating routine (called \refgen{01}) in CSound that takes an
argument type other than floating point, and thus we represent this
using the special constructor {\tt SoundFile}, whose functionality
will be described shortly.

Knowing which of CSound's generating routines to use and with what
arguments can be a daunting task.  The newest version of CSound
(version 4.01) provides 23 different generating routines, and each one
of them assigns special meanings to its arguments.  To avoid having to
reference routines using integer ids, the following functions are
defined for the most often-used generating routines.  A brief
discussion of each routine is also included.  For a full description
of these and other routines, refer to the CSound manual or consult the
following webpage:
\url{http://www.leeds.ac.uk/music/Man/Csound/Function/GENS.html}.
The user
familiar with CSound is free to write helper functions like the ones
below to capture other generating routines.

\genparagraph{01} Transfers data from a soundfile into a function
table.  Recall that the size of the function table in CSound must be a
power of two.  If the soundfile is larger than the table size, reading
stops when the table is full; if it is smaller, then the table is
padded with zeros.  One exception is allowed: if the file is of type
AIFF and the table size is set to zero, the size of the function table
is allocated dynamically as the number of points in the soundfile.
The table is then unusable by normal oscillators, but can be used by a
special {\tt SampOsc} constructor (discussed in \secref{orchestra-file}).  The first argument passed to the \refgen{01}
subroutine is a string containing the name of the source file.  The
second argument is skip time, which is the number of seconds into the
file that the reading begins.  Finally there is an argument for the
channel number, with 0 meaning read all channels.  \refgen{01} is
represented in Haskore as {\tt SoundFile SFName SkipTime ChanNum}, as
discussed earlier.  To make the use of {\tt SoundFile} consistent with
the use of other functions to be described shortly, we define a simple
equivalent:
\begin{haskelllisting}

> soundFile :: SFName -> SkipTime -> ChanNum -> T
> soundFile = SoundFile

\end{haskelllisting}

\genparagraph{02} Transfers data from its argument fields directly
into the function table.  We represent its functionality as follows:
\begin{haskelllisting}

> tableValues :: [Parameter] -> T
> tableValues gas = Routine 2 gas

\end{haskelllisting}

\genparagraph{03} Fills the table by evaluating a polynomial over a
specified interval and with given coefficients.  For example, calling
\refgen{03} with an interval of $(-1,1)$ and coefficients 5, 4, 3, 2, 0, 1
will generate values of the function $5+4x+3x^2+2x^3+x^5$ over the
interval $-1$ to $1$.  The number of values generated is equal to the
size of the table.  Let's express this by the following function:
\begin{haskelllisting}

> polynomial :: Interval -> Coefficients -> T
> polynomial (x1,x2) cfs = Routine 3 (x1:x2:cfs)
>
> type Interval     = (Float, Float)
> type Coefficients = [Float]

\end{haskelllisting}

\genparagraph{05} Constructs a table from segments of exponential
curves.  The first argument is the starting point.  The meaning of the
subsequent arguments alternates between the length of a segment in
samples, and the endpoint of the segment.  The endpoint of one segment
is the starting point of the next.  The sum of all the segment lengths
normally equals the size of the table: if it is less the table is
padded with zeros, if it is more, only the first \type{TableSize}
locations will be stored in the table.

\begin{haskelllisting}

> exponential1 :: StartPt -> [(SegLength, EndPt)] -> T
> exponential1 sp xs = Routine 5 (sp : flattenTuples2 xs)
>
> type StartPt   = Float
> type SegLength = Float
> type EndPt     = Float

\end{haskelllisting}

\genparagraph{25} Similar to \refgen{05} in that it produces segments of
exponential curves, but instead of representing the lengths of
segments and their endpoints, its arguments represent $(x,y)$
coordinates in the table, and the subroutine produces curves between
successive locations.  The $x$-coordinates must be in increasing
order.

\begin{haskelllisting}

> exponential2 :: [Point] -> T
> exponential2 pts = Routine 25 (flattenTuples2 pts)
>
> type Point = (Float,Float)

\end{haskelllisting}

\genparagraph{06} Generates a table from segments of cubic
polynomial functions, spanning three points at a time.  We define a
function {\tt cubic} with two arguments: a starting position and a
list of segment length (in number of samples) and segment endpoint
pairs.  The endpoint of one segment is the starting point of the next.
The meaning of the segment endpoint alternates between a local
minimum/maximum and point of inflexion.  Whether a point is a maximum
or a minimum is determined by its relation to the next point of
inflexion.  Also note that for two successive minima or maxima, the
inflexion points will be jagged, whereas for alternating maxima and
minima, they will be smooth.  The slope of the two segments is
independent at the point of inflection and will likely vary.  The
starting point is a local minimum or maximum (if the following point
is greater than the starting point, then the starting point is a
minimum, otherwise it is a maximum).  The first pair of numbers will
in essence indicate the position of the first inflexion point in
$(x,y)$ coordinates.  The folowing pair will determine the next local
minimum/maximum, followed by the second point of inflexion, etc.
\begin{haskelllisting}

> cubic ::  StartPt -> [(SegLength, EndPt)] -> T
> cubic sp pts = Routine 6 (sp : flattenTuples2 pts)

\end{haskelllisting}

\genparagraph{07} Similar to \refgen{05}, except that it generates
straight lines instead of exponential curve segments.  All other
issues discussed about \refgen{05} also apply to \refgen{07}.  We represent it as:
\begin{haskelllisting}

> lineSeg1 :: StartPt -> [(SegLength, EndPt)] -> T
> lineSeg1 sp pts = Routine 7 (sp : flattenTuples2 pts)

\end{haskelllisting}

\genparagraph{27} As with \refgen{05} and \refgen{25}, produces straight line
segments between points whose locations are given as $(x,y)$
coordinates, rather than a list of segment length, endpoint pairs.
\begin{haskelllisting}

> lineSeg2 :: [Point] -> T
> lineSeg2 pts = Routine 27 (flattenTuples2 pts)

\end{haskelllisting}

\genparagraph{08} Produces a smooth piecewise cubic spline curve
through the specified points.  Neighboring segments have the same
slope at the common points, and it is that of a parabola through that
point and its two neighbors.  The slope is zero at the ends.
\begin{haskelllisting}

> cubicSpline :: StartPt -> [(SegLength, EndPt)] -> T
> cubicSpline sp pts = Routine 8 (sp : flattenTuples2 pts)

\end{haskelllisting}

\genparagraph{10} Produces a composite sinusoid.  It takes a list of
relative strengths of harmonic partials 1, 2, 3, etc.  Partials not
required should be given strength of zero.
\begin{haskelllisting}

> compSine1 :: [PStrength] -> T
> compSine1 pss = Routine 10 pss
>
> type PStrength = Float

\end{haskelllisting}

\genparagraph{09} Also produces a composite sinusoid, but requires
three arguments to specify each contributing partial.  The arguments
specify the partial number, which doesn't have to be an integer (i.e.\
inharmonic partials are allowed), the relative partial strength, and
the initial phase offset of each partial, expressed in degrees.
\begin{haskelllisting}

> compSine2 :: [(PNum, PStrength, PhaseOffset)] -> T
> compSine2 args = Routine 9 (flattenTuples3 args)
>
> type PNum = Float
> type PhaseOffset = Float

\end{haskelllisting}

\genparagraph{19} Provides all of the functionality of \refgen{09}, but in
addition a DC offset must be specified for each partial.  The DC
offset is a vertical displacement, so that a value of 2 will lift a
2-strength partial from range $[-2,2]$ to range $[0,4]$ before further
scaling.
\begin{haskelllisting}

> compSine3 :: [(PNum, PStrength, PhaseOffset, DCOffset)] -> T
> compSine3 args = Routine 19 (flattenTuples4 args)
>
> type DCOffset = Float

\end{haskelllisting}

\genparagraph{11} Produces an additive set of harmonic cosine
partials, similar to \refgen{10}.  We will represent it by a function that
takes three arguments: the number of harmonics present, the lowest
harmonic present, and a multiplier in an exponential series of
harmonics amplitudes (if the $x$'th harmonic has strength coefficient
of $A$, then the $(x+n)$'th harmonic will have a strength of
$A*(r^n)$, where $r$ is the multiplier).
\begin{haskelllisting}

> cosineHarms :: NHarms -> LowestHarm -> Mult -> T
> cosineHarms n l m = Routine 11 [fromIntegral n, fromIntegral l, m]
>
> type NHarms = Int
> type LowestHarm = Int
> type Mult = Float

\end{haskelllisting}

\genparagraph{21} Produces tables having selected random distributions.
\begin{haskelllisting}

> randomTable :: RandDist -> T
> randomTable rd = Routine 21 [fromIntegral (fromEnum rd + 1)]
>
> data RandDist =
>        Uniform
>      | Linear
>      | Triangular
>      | Expon
>      | BiExpon
>      | Gaussian
>      | Cauchy
>      | PosCauchy
>    deriving (Eq, Ord, Enum, Show)

\end{haskelllisting}

\begin{haskelllisting}

> toStatementWords :: T -> [String]
> toStatementWords (Routine gn gas)     = show gn : map show gas
> toStatementWords (SoundFile nm st cn) = ["1", nm, show st, "0", show cn]

\end{haskelllisting}
