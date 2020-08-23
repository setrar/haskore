\subsubsection{The Orchestra File}
\seclabel{orchestra-file}

\newcommand\csoundfunc[1]{\textit{\texttt{#1}}}

\begin{haskelllisting}

> module Haskore.Interface.CSound.Orchestra (
>    T(Cons), InstrBlock(..), Header, AudRate, CtrlRate,
>    -- SigTerm(ConstFloat, ConstInt, TableNumber, PField, Str,
>    --         Read, Tap, Result, Conditional,
>    --         Infix, Prefix, SigGen),
>    SigExp, DelayLine, Boolean,
>    -- DelayLine(DelayLine), Boolean(Operator, Comparison),
>    GlobalSig(Global), Output(..), Mono(Mono), Stereo(Stereo), Quad(Quad),
>    EvalRate(NR, CR, AR), Instrument, Name,
>    sigGen, tableNumber, readGlobal, rec,
>
>    -- assorted functions
>    toString, saveIA, save,
>    channelCount, getMultipleOutputs,
>
>    -- variables dealing with PFields
>    noteDur, notePit, noteVel, p1, p2, p3, p4, p5, p6, p7, p8, p9, pField,
>
>    -- functions for dealing with Booleans and Conditional SigExps
>    (<*), (<=*), (>*), (>=*), (==*), (/=*), (&&*), (||*), ifthen,
>    constInt, constFloat, constEnum,
>
>    -- functions for creating signal expressions
>    pchToHz, dbToAmp, line, expon, lineSeg, exponSeg, env, phasor,
>    IndexMode(..), tblLookup, tblLookupI, osc, oscI,
>    fmOsc, fmOscI, sampOsc, random, randomH, randomI, genBuzz, buzz,
>    pluck, PluckDecayMethod(..), delay, vdelay, comb, alpass, reverb,
>    delTap, delTapI,
>
>    -- monad-related functions
>    Orc, mkSignal, addInstr, mkOrc,
>
>    -- assorted examples
>    orc1, test, test1) where
>
> import Haskore.Interface.CSound
>           (Name, Instrument, instrument, instruments, showInstrumentNumber)
> import Haskore.Interface.CSound.OrchestraFunction
>
> import qualified Haskore.General.LoopTreeRecursiveGen as TreeRec
> import qualified Haskore.General.LoopTreeTaggedGen    as TreeTag

> import System.IO (IO, FilePath, getLine, putStr, writeFile, )

> import Control.Monad.Trans.State (State, state, modify, execState, )
> import Control.Applicative (liftA, liftA2, liftA3, pure)
> import Data.Foldable (Foldable(foldMap))
> import Data.Traversable (Traversable(sequenceA))
> import qualified Data.Traversable as Traversable

> import Haskore.General.Utility (flattenTuples2, )
> import Data.List.HT (partition, )
> import Data.List
>    (nub, intersperse, concatMap, concat, map, zipWith,
>     filter, head, unlines, length, null, reverse, lookup, (\\), (++), )
> import Data.Maybe.HT (toMaybe, )
> import Data.Maybe (Maybe(Just, Nothing), mapMaybe, maybe, )
> import Data.Bool (Bool(False, True), not, (&&), )
> import Data.Tuple.HT (mapSnd, )
> import Data.Tuple (fst, snd, )
> import Data.Ord (Ord, max, )
> import Data.Eq (Eq, (==), (/=), )
> import Data.Functor (Functor, fmap, )
> import Data.Function (id, flip, ($), (.), )

> import Text.Show (Show, showsPrec, show, )

> import Prelude (Floating(..), Fractional(..), Num(..), )
> import Prelude (Integer, Int, Float, Double, fromIntegral, )
> import Prelude (Enum, fromEnum, )
> import Prelude (String, error, )

\end{haskelllisting}

The orchestra file consists of two parts: a \keyword{header}, and one or
more \keyword{instrument blocks}.  The header sets global parameters
controlling sampling rate and control rate.  The instrument blocks define
instruments, each identified by a unique integer ID, and containing
statements modifying or generating various audio signals.  Each note statement
in a score file passes all its arguments---including the p-fields---to its
corresponding instrument in the orchestra file.  While some properties
vary from note to note, and should therefore be designed as p-fields,
many can be defined within the instrument; the choice is up to the
user.

The orchestra file is represented as:
\begin{haskelllisting}

> data -- Output out =>
>         T out = Cons Header [InstrBlock out] deriving (Show, Eq)

\end{haskelllisting}
The orchestra header sets the audio rate, control rate, and number of
output channels:
\begin{haskelllisting}

> type Header = (AudRate, CtrlRate)
>
> type AudRate  = Int  -- samples per second
> type CtrlRate = Int  -- samples per second

\end{haskelllisting}
Digital computers represent continuous analog audio waveforms as a
sequence of discrete samples.  The audio rate (\type{AudRate}) is the
number of these samples calculated each second.  Theoretically, the
maximum frequency that can be represented is equal to one-half the
audio rate.  Audio CDs contain 44,100 samples per second of music,
giving them a maximum sound frequency of 22,050 Hz, which is as high
as most human ears are able to hear.

Computing 44,100 values each second can be a demanding task for a CPU,
even by today's standards.  However, some signals used as inputs to
other signal generating routines don't require such a high resolution,
and can thus be generated at a lower rate.  A good example of this is
an amplitude envelope, which changes relatively slowly, and thus can
be generated at a rate much lower than the audio rate.  This rate is
called the \keyword{control rate} (\type{CtrlRate}), and is set in the
orchestra file header.  The audio rate is usually a multiple of the
control rate, but this is not a requirement.

Each instrument block contains four things: a unique identifying
integer; an expression giving the amount of extra time the instrument
should be granted, usually used for reverb; an \type{Output} expression
that gives the outputs in terms of \keyword{orchestra expressions},
called \type{SigExp}s; and a list of global signals and the \type{SigExp}s
that are written out to those signals.
\begin{haskelllisting}

> type Reverb = SigExp
> data InstrBlock a =
>        InstrBlock {instrBlockInstr   :: Instrument,
>                    instrBlockReverb  :: Reverb,
>                    instrBlockOutput  :: a,
>                    instrBlockGlobals :: [(GlobalSig, SigExp)]}
>      deriving (Show, Eq)

\end{haskelllisting}
Recall that \type{Instrument} is a type synonym for an \type{Int}.  This value
may be obtained from a string name and a name map using the function
\code{getId :: NameMap -> Name -> Maybe Int} discussed earlier.

\paragraph{Orchestra Expressions}

The data type \type{SigExp} is the largest deviation that we will make
from the actual CSound design.  In CSound, instruments are defined
using a sequence of statements that, in a piecemeal manner, define the
various oscillators, summers, constants, etc.\ that make up an
instrument.  These pieces can be given names, and these names can be
referenced from other statements.  But despite this rather imperative,
statement-oriented approach, it is acually completely functional.  In
other words, every CSound instrument can be rewritten as a single
expression.  It is this ``expression language'' that we capture in
\type{SigExp}.  A pleasant attribute of the result is that CSound's ad
hoc naming mechanism is replaced with Haskell's conventional way of
naming things.

The entire \type{SigExp} data type declaration, as well as the
declarations for related datatypes, is shown in \figref{SigExp}.
In what follows, we describe each of the various constructors in turn.

\begin{figure}
{\scriptsize\vspace{-.7in}
\begin{haskelllisting}

> type Function = String
> type OutCount = Integer
> type Table    = Int
>
> type Boolean = BooleanTerm SigExp
> data BooleanTerm tree =
>            Operator   Function (BooleanTerm tree) (BooleanTerm tree)
>          | Comparison Function tree tree
>      deriving (Show, Eq)
>
> data GlobalSig =
>    Global EvalRate (SigExp -> SigExp -> SigExp) Int
> instance Show GlobalSig where
>    show (Global rt _ n) = "Global " ++ show rt ++ " <function> " ++ show n
> instance Eq GlobalSig where
>    Global r1 _ n1 == Global r2 _ n2  =  r1 == r2 && n1 == n2
>
> type DelayLine = DelayLineTerm SigExp
> data DelayLineTerm tree = DelayLine tree tree
>      deriving (Show, Eq)
>
> data SigTerm tree =
>               ConstFloat Float
>             | ConstInt Int
>             | TableNumber Table
>             | PField Int
>             | Str String
>             | Read GlobalSig
>             | Tap Function (DelayLineTerm tree) [tree]
>             | Result (DelayLineTerm tree)
>             | Conditional (BooleanTerm tree) tree tree
>             | Infix  Function tree tree
>             | Prefix Function tree
>             | SigGen Function EvalRate OutCount [tree]
>             | Index OutCount (SigTerm tree)
>      deriving (Show, Eq)

> instance Functor BooleanTerm where
>    fmap f branch =
>       case branch of
>         Operator   nm left right -> Operator   nm (fmap f left) (fmap f right)
>         Comparison nm left right -> Comparison nm (     f left) (     f right)
>
> instance Functor DelayLineTerm where
>    fmap f (DelayLine x y) = DelayLine (f x) (f y)

> instance Functor SigTerm where
>    fmap f branch =
>      case branch of
>         {- The first cases look like they could be handled
>            by returning just 'branch'. But this does not work,
>            because the result have a different type in general. -}
>         ConstFloat x       -> ConstFloat x
>         ConstInt n    -> ConstInt n
>         TableNumber t -> TableNumber t
>         PField n      -> PField n
>         Str str       -> Str str
>         Read t        -> Read t
>         Tap nm del xs -> Tap nm (fmap f del) (map f xs)
>         Result del    -> Result (fmap f del)
>         Conditional b true false ->
>            Conditional (fmap f b) (f true) (f false)
>         Infix  nm left right -> Infix nm (f left) (f right)
>         Prefix nm arg        -> Prefix nm (f arg)
>         SigGen nm rate cnt args ->
>            SigGen nm rate cnt (map f args)
>         Index cnt x -> Index cnt (fmap f x)

> instance TreeTag.CollShow SigTerm where
>    collShowsPrec  =  showsPrec

> instance TreeTag.CollEq SigTerm where
>    collEqual  =  (==)

> type SigExp = TreeRec.T SigTerm

> tableNumber :: Table -> SigExp
> tableNumber n = TreeRec.Branch (TableNumber n)

> readGlobal :: GlobalSig -> SigExp
> readGlobal glob = TreeRec.Branch (Read glob)

\end{haskelllisting}
}
\caption{The \type{SigExp} Data Type}
\figlabel{SigExp}
\end{figure}


\subparagraph{Constants}

\code{ConstFloat x} represents the floating-point constant \code{x}.

\subparagraph{P-field Arguments}

\code{pField n} refers to the $n$th p-field argument.  Recall that all
note characteristics, including pitch, volume, and duration, are
passed into the orchestra file as p-fields.  For example, to access
the pitch, one would write \code{pField 4}.  To make the access of
these most common p-fields easier, we define the following constants:
\begin{haskelllisting}

> noteDur, notePit, noteVel :: SigExp
> noteDur = pField 3
> notePit = pField 4
> noteVel = pField 5

> pField :: Int -> SigExp
> pField n = TreeRec.Branch (PField n)

\end{haskelllisting}

It is also useful to define the following standard names, which are
identical to those used in CSound:

\begin{haskelllisting}

> p1,p2,p3,p4,p5,p6,p7,p8,p9 :: SigExp
> p1 = pField 1
> p2 = pField 2
> p3 = pField 3
> p4 = pField 4
> p5 = pField 5
> p6 = pField 6
> p7 = pField 7
> p8 = pField 8
> p9 = pField 9

\end{haskelllisting}

\subparagraph{Strings}

\code{Str s} represents a string argument in CSound --- a type of argument that
is very rarely used, but is included here for the sake of completeness.

\paragraph{Reading and Writing Global Signals}

\code{Read g} is the counterpart to the \type{(GlobalSig, SigExp)} pairs in the
\type{InstrBlock} statements, reading instead of writing global signals.  Together,
they allow for audio and control signals to be passed from instrument to instrument,
and used for things like panning or overall envelopes.

\paragraph{Logical and Conditional Statements}

You probably noticed that \type{Boolean} was defined alongside
\type{SigExp} in Figure \ref{SigExp-fig}.  \type{Boolean} is a type of
expression used in the \constructor{Conditional} \type{SigExp} --- basically,
it's a comparison or some logical function of two comparisons.  In other
words, a \type{Boolean} is an expression that evaluates to a boolean.
The syntax is fairly simple --- a \type{Boolean} is either a \constructor{Comparison},
a function comparing two \type{SigExp}s and returning a \type{Boolean};
or an \constructor{Operator}, a function from two \type{Boolean}s to a third
\type{Boolean}, such as the logical ``and'' operator. Thus we can express,
for example, a query about whether a certain p-value lies within a range
by evaluating this expression:

\begin{haskelllisting}

  Operator "&&" (Comparison "<" 1 p2) (Comparison "<" p2 3)

\end{haskelllisting}

The above expression will create a CSound expression that is true when p2
lies between 1 and 3.

\type{Boolean}s can be used inside of a \constructor{Conditional} expression in order
to choose one of two values based on the trueness or falseness of the
\type{Boolean}.  For example:

\begin{haskelllisting}

  Conditional (Comparison ">" p1 p2) p1 p2

\end{haskelllisting}

will return the maximum of the two values p1 and p2.  We are including
several functions that will perform this automatically:

\begin{haskelllisting}

> (<*), (<=*), (>*), (>=*), (==*), (/=*) ::
>    -- SigExp -> SigExp -> Boolean
>    TreeTerm term =>
>       TreeRec.T term -> TreeRec.T term -> BooleanTerm (TreeRec.T term)
> (<* ) = comparisonTerm "<"
> (<=*) = comparisonTerm "<="
> (>* ) = comparisonTerm ">"
> (>=*) = comparisonTerm ">="
> (==*) = comparisonTerm "=="
> (/=*) = comparisonTerm "!="

> (&&*), (||*) :: Boolean -> Boolean -> Boolean
> (&&*) = operator   "&&"
> (||*) = operator   "||"

> operator :: String -> Boolean -> Boolean -> Boolean
> operator = Operator

\end{haskelllisting}

\subparagraph{Arithmetic and Transcendental Functions}

Arithmetic functions are represented in various ways, depending on the
type of function.  The standard binary operators --- plus and times, for
instance --- are infix operators, and so they can be crafted in this
module using the Infix constructor, specifying the name of the
function (the text used to express it in CSound) and the two arguments
to the function.  The other mathematical operators, such as \function{sin},
\function{log}, or \function{sqrt}, can be expressed with a \constructor{Prefix} constructor,
passing the name of the function in CSound (usually the same as the name
in Haskell, although not always) and the argument to the given function.
Examples of this are:

\begin{verbatim}
Infix "+" (PField 1) (Prefix "sin" 1 (ConstFloat 3.0))
Prefix "sqrt" (Infix "*" (PField 3) (PField 4))
\end{verbatim}

To facilitate the use of these arithmetic functions, we can make
\type{SigExp} an instance of certain numeric type classes, thus providing
more conventional names for the various operations.
\begin{haskelllisting}

> sigGen :: Function -> EvalRate -> OutCount -> [SigExp] -> SigExp
> sigGen nm rate cnt args = TreeRec.Branch (SigGen nm rate cnt args)

> constFloat :: Float -> SigExp
> constFloat = TreeRec.Branch . ConstFloat

> constInt :: Int -> SigExp
> constInt = TreeRec.Branch . ConstInt

> constEnum :: Enum a => a -> SigExp
> constEnum = TreeRec.Branch . ConstInt . fromEnum

> class TreeTerm term where
>   constTerm  :: Float    -> TreeRec.T term
>   prefixTerm :: Function -> TreeRec.T term -> TreeRec.T term
>   infixTerm  :: Function -> TreeRec.T term -> TreeRec.T term -> TreeRec.T term
>   comparisonTerm  :: Function -> TreeRec.T term -> TreeRec.T term ->
>                                                 BooleanTerm (TreeRec.T term)
>   ifthen :: BooleanTerm (TreeRec.T term) ->
>                           TreeRec.T term -> TreeRec.T term -> TreeRec.T term

> instance TreeTerm SigTerm where
>   constTerm          x   = TreeRec.Branch (ConstFloat          x)
>   prefixTerm      nm x   = TreeRec.Branch (Prefix      nm x)
>   infixTerm       nm x y = TreeRec.Branch (Infix       nm x y)
>   comparisonTerm  nm x y = Comparison nm x y
>   ifthen          b  x y = TreeRec.Branch (Conditional b  x y)

\end{haskelllisting}

We can not request \code{term == SigTerm TreeRec.T}
that's why we have to define the \code{TreeTerm} class
and the instance for \code{SigTerm}.

\begin{haskelllisting}

> instance (TreeTag.CollShow term, TreeTag.CollEq term,
>           Functor term, TreeTerm term) =>
>          Num (TreeRec.T term) where
>   (+)      = infixTerm "+"
>   (-)      = infixTerm "-"
>   (*)      = infixTerm "*"
>   negate   = prefixTerm "-"
>   abs      = prefixTerm "abs"
>   signum x = ifthen (x <* 0) (-1) (ifthen (x >* 0) 1 0)
>   fromInteger = constTerm . fromInteger

> instance (TreeTag.CollShow term, TreeTag.CollEq term,
>           Functor term, TreeTerm term) =>
>          Fractional (TreeRec.T term) where
>   (/) = infixTerm "/"
>   fromRational = constTerm . fromRational
> {-
>   fromRational x =
>      fromInteger (numerator x) /
>      fromInteger (denominator x)
> -}

> instance (TreeTag.CollShow term, TreeTag.CollEq term,
>           Functor term, TreeTerm term) =>
>          Floating (TreeRec.T term) where
>   exp     = prefixTerm "exp"
>   log     = prefixTerm "log"
>   sqrt    = prefixTerm "sqrt"
>   (**)    = infixTerm "^"
>   pi      = constTerm pi
>   sin     = prefixTerm "sin"
>   cos     = prefixTerm "cos"
>   tan     = prefixTerm "tan"
>   asin    = prefixTerm "sininv"
>   acos    = prefixTerm "cosinv"
>   atan    = prefixTerm "taninv"
>   sinh    = prefixTerm "sinh"
>   cosh    = prefixTerm "cosh"
>   tanh    = prefixTerm "tanh"
>   asinh x = log (sqrt (x*x+1) + x)
>   acosh x = log (sqrt (x*x-1) + x)
>   atanh x = (log (1+x) - log (1-x)) / 2

\end{haskelllisting}

Now we can write simpler code, such as: \code{noteDur + sin p6 ** 2}.

\paragraph{Other \constructor{Prefix}s}

\function{sin}, \function{log}, and \function{sqrt} aren't the only functions that
use \constructor{Prefix} as a constructor --- \constructor{Prefix} is used
for all functions in CSound that take a single argument and are represented
like normal mathematical functions.  Most of these functions are, indeed,
mathematical, such as the function converting a CSound pitch value to
the number of cycles per second, or the function converting decibels
to the corresponding amplitude.

For convenience, we will define a few common operators here:

\begin{verbatim}

> pchToHz, dbToAmp :: SigExp -> SigExp
> pchToHz = prefixTerm "cpspch"
> dbToAmp = prefixTerm "ampdb"

\end{verbatim}

Now, when we want to convert a pitch to its hertz value or a decibel
level to the desired amplitude, we can simply say \code{pchToHz notePit}
or \code{dbToAmp noteVel}.

\paragraph{Signal Generation and Modification}

The most sophisticated \type{SigExp} constructor is \function{sigGen},
which drives most of the functions used for signal generation and
modification.  The constructor takes four arguments: the name of the
function to be used, such as \csoundfunc{envlpx} or \csoundfunc{oscili}; the rate of output;
the number of outputs (covered in a later section); and a list of all
the arguments to be passed.

Most of these we have seen before.  But what is the rate of output?  Well,
signals in CSound can be generated at three rates: the note rate (i.e.,
with, every note event), the control rate, and the audio rate (we discussed
the latter two earlier).  Many of the signal generating routines can
produce signals at more than one rate, so the rate must be specified
as an argument.  The following simple data structure serves this purpose:

\begin{haskelllisting}

> data EvalRate = NR  -- note rate
>               | CR  -- control rate
>               | AR  -- audio rate
>      deriving (Show, Eq, Ord)

\end{haskelllisting}

All right, so now we know what the arguments are.  But what does the
\function{sigGen} constructor actually do?  Like the other kinds of
\type{SigExp}s, it has an input and an output.  In Haskore, it acts
just the same as any other kind of function.  But when written to a CSound
Orchestra file, each \function{sigGen} receives a variable name that it is
assigned to, and each \function{sigGen} is written to a single line of the
CSound file.

\function{sigGen}s can be used for all sorts of things --- CSound has a
very large variety of functions, most of which are actually \function{sigGen}s.
They can do anything from generating a simple sine wave to generating complex
signals.  Most of them, however, have to do with signal generation; hence the
name \function{sigGen}.  For the user's sake, we will outline a few of the CSound
functions here:

\begin{enumerate}
\item The CSound statement \code{line evalrate start duration finish},
produces values along a
straight line from \code{start} to \code{finish}.  The values can be
generated either at control or audio rate, and the line covers a
period of time equal to \code{duration} seconds.  We can translate this into
CSound like so:
\begin{haskelllisting}

> line, expon :: EvalRate -> SigExp -> SigExp -> SigExp -> SigExp
> line rate start duration finish =
>    sigGen "line" rate 1 [start, duration, finish]

\end{haskelllisting}

\item \csoundfunc{expon} is similar to \csoundfunc{line},
but the code \code{expon evalrate start duration finish}
produces an exponential curve instead of a straight line.
\begin{haskelllisting}

> expon rate start duration finish =
>    sigGen "expon" rate 1 [start, duration, finish]

\end{haskelllisting}

\item If a more elaborate signal is required, one can use the
CSound functions \csoundfunc{linseg} or \csoundfunc{expseg}, which take any
odd number of arguments greater than or equal to three.  The first three
arguments work as before, but only for the first of a number of segments.
The subsequent segment lengths and endpoints are given in the rest of the
arguments.  A signal containing both straight line and exponential
segments can be obtained by adding a \csoundfunc{linseg} signal and
\csoundfunc{expseg} signal together in an appropriate way.

The Haskore code is more complicated for this,
because there are an arbitrary but odd number of arguments.
So we will give the first three arguments as we did with the \csoundfunc{line}
and \csoundfunc{expon} functions, and then have a list of pairs, which will be
flattened into an argument list:
\begin{haskelllisting}

> lineSeg, exponSeg :: EvalRate -> SigExp -> SigExp -> SigExp
>                       -> [(SigExp, SigExp)] -> SigExp
> lineSeg rate y0 x1 y1 lst =
>    sigGen "linseg" rate 1 ([y0, x1, y1] ++ flattenTuples2 lst)
> exponSeg rate y0 x1 y1 lst =
>    sigGen "expseg" rate 1 ([y0, x1, y1] ++ flattenTuples2 lst)

\end{haskelllisting}

\item The Haskore code
\code{env rate rshape sattn dattn steep dtime rtime durn sig}
modifies the signal \code{sig} by applying an envelope to it.%
\footnote{Although this function is widely-used in CSound, the same
effect can be accomplished by creating a signal that is a combination
of straight line and exponential curve segments, and multiplying it by
the signal to be modified.}
\code{rtime} and \code{dtime} are the rise
time and decay time, respectively (in seconds), and \code{durn} is the
overall duration.  \code{rshape} is the identifier integer of a
function table storing the rise shape.  \code{sattn} is the
pseudo-steady state attenuation factor.  A value between 0 and 1 will
cause the signal to exponentially decay over the steady period, a
value greater than 1 will cause the signal to exponentially rise, and
a value of 1 is a true steady state maintained at the last rise value.
\code{steep}, whose value is usually between $-0.9$ and $+0.9$,
influences the steepness of the exponential trajectory.  \code{dattn}
is the attenuation factor by which the closing steady state value is
reduced exponentially over the decay period, with value usually around
0.01.

In Haskore, this becomes a fairly simple function, going from an
\type{EvalRate} and eight \type{SigExp}s to one single \type{SigExp}:
\begin{haskelllisting}

> env :: EvalRate -> SigExp -> SigExp -> SigExp -> SigExp -> SigExp
>         -> SigExp -> SigExp -> SigExp -> SigExp
> env rate rshape sattn dattn steep dtime rtime durn sig =
>    sigGen "envlpx" rate 1
>       [sig, rtime, durn, dtime, rshape, sattn, dattn, steep]

\end{haskelllisting}

\item Typing \code{phasor phase freq} into CSound generates a signal moving
from 0 to 1 at a given frequency and starting at the given initial
phase offset.  When used properly as the index to a table lookup unit,
the function can simulate the behavior of an oscillator.  We implement it
in Haskore thus:
\begin{haskelllisting}

> phasor :: EvalRate -> SigExp -> SigExp -> SigExp
> phasor rate phase freq = sigGen "phasor" rate 1 [freq, phase]

\end{haskelllisting}

\item CSound table lookup functions \csoundfunc{table} and \csoundfunc{tablei}
both take \code{index}, \code{table}, and \code{indexmode} arguments.
The \code{indexmode} is either 0 or 1,
differentiating between raw index and normalized index (zero to one);
for convenience we define:
\begin{haskelllisting}

> data IndexMode =
>      RawIndex
>    | NormalIndex
>        deriving (Show, Eq, Enum)

\end{haskelllisting}

Both \csoundfunc{table} and \csoundfunc{tablei} return values stored in
the specified table at the given index.
The difference is that \csoundfunc{tablei}
uses the fractional part of the index to interpolate
between adjacent table entries, which generates a smoother signal at a
small cost in execution time.  The equivalent Haskore code to the CSound
functions is:
\begin{haskelllisting}

> tblLookup, tblLookupI ::
>    EvalRate -> IndexMode -> SigExp -> SigExp -> SigExp
> tblLookup  rate mode table ix =
>    sigGen "table"  rate 1 [ix, table, constEnum mode]
> tblLookupI rate mode table ix =
>    sigGen "tablei" rate 1 [ix, table, constEnum mode]

\end{haskelllisting}

As mentioned, the output of \csoundfunc{phasor} can be used as input to a
table lookup to simulate an oscillator whose frequency is controlled
by the note pitch.  This can be accomplished easily by the following
piece of Haskore code:
\begin{haskelllisting}

  oscil = let index = phasor AR (pchToHz notePit) 0.0
          in  tblLookupI AR NormalIndex table index

\end{haskelllisting}
where \code{table} is some given function table ID.  If \code{oscil} is
given as argument to an output constructor such as \constructor{MonoOut}, then
this \type{Output} coupled with an instrument ID number (say, 1)
produces a complete instrument block:
\begin{haskelllisting}

  i1 = (1, MonoOut oscil)

\end{haskelllisting}
Adding a suitable \constructor{Header} would then give us a complete, though
somewhat sparse, \type{CSound.Orchestra.T} value.

\item Instead of the above design we could use one of the built-in
CSound oscillators, \csoundfunc{oscil} and \csoundfunc{oscili}, which differ
in the same way as \csoundfunc{table} and \csoundfunc{tablei}.  Both CSound
functions take the following arguments: raw amplitude, frequency, and
the index of a table.  The result is a signal that oscillates through
the function table at the given frequency.  Let the Haskore functions
be as follows:
\begin{haskelllisting}

> osc, oscI :: EvalRate -> SigExp -> SigExp -> SigExp -> SigExp
> osc  rate table amp freq = sigGen "oscil"  rate 1 [amp, freq, table]
> oscI rate table amp freq = sigGen "oscili" rate 1 [amp, freq, table]

\end{haskelllisting}
Now, the following statement is equivalent to \function{osc}, defined above:
\begin{haskelllisting}
  oscil' = oscI AR 1 (pchToHz notePit) table
\end{haskelllisting}

\item It is often desirable to use the output of one oscillator to
modulate the frequency of another, a process known as \keyword{frequency
modulation}.
The Haskore code \code{fmOsc table modindex carfreq modfreq amp freq}
produces a signal whose effective modulating frequency is \code{freq*modfreq},
and whose carrier frequency is \code{freq*carfreq}.  \code{modindex} is the
\keyword{index of modulation}, usually a value between 0 and 4, which
determines the timbre of the resulting signal.  \csoundfunc{oscili}
behaves similarly to \csoundfunc{oscil}, except that it, like \csoundfunc{tablei}
and \csoundfunc{oscili}, interpolates between values.

Interestingly enough, these two functions are the first listed here that
work at audio rate only; thus, we do not have to pass the rate as an argument
to the helper function, because the rate is always \constructor{AR}.  Thus, the Haskore
code is:
\begin{haskelllisting}

> fmOsc, fmOscI :: SigExp -> SigExp -> SigExp -> SigExp -> SigExp
>                   -> SigExp -> SigExp
> fmOsc  table modindex carfreq modfreq amp freq =
>    sigGen "foscil"  AR 1 [amp, freq, carfreq, modfreq, modindex, table]
> fmOscI table modindex carfreq modfreq amp freq =
>    sigGen "foscili" AR 1 [amp, freq, carfreq, modfreq, modindex, table]

\end{haskelllisting}

\item \code{sampOsc table amp freq} oscillates through a table
containing an AIFF sampled sound segment.  This is the only time a
table can have a length that is not a power of two, as mentioned
earlier.  Like \function{fmOsc}, \function{sampOsc} can only generate values at
the audio rate:
\begin{haskelllisting}

> sampOsc :: SigExp -> SigExp -> SigExp -> SigExp
> sampOsc table amp freq = sigGen "loscil" AR 1 [amp, freq, table]

\end{haskelllisting}

\item The Haskore code \code{random rate amp} produces a random number series
between \code{-amp} and \code{+amp} at either control or audio rate.
\code{randomH rate quantRate amp} does the same but will hold each
number for \code{quantRate} cycles before generating a new one.
\code{randomI rate quantRate amp} will in addition provide straight
line interpolation between successive numbers:
\begin{haskelllisting}

> random :: EvalRate -> SigExp -> SigExp
> random rate amp = sigGen "rand" rate 1 [amp]

> randomH, randomI :: EvalRate -> SigExp -> SigExp -> SigExp
> randomH rate quantRate amp = sigGen "randh" rate 1 [amp, quantRate]
> randomI rate quantRate amp = sigGen "randi" rate 1 [amp, quantRate]

\end{haskelllisting}

The remaining functions covered in this file only operate at audio rate,
and thus their Haskore equivalents do not have \code{rate} arguments.

\item \code{genBuzz table multiplier loharm numharms amp freq}
generates a signal that is an additive set of harmonically related
cosine partials.  \code{freq} is the fundamental frequency,
\code{numharms} is the number of harmonics, and \code{loharm} is the lowest
harmonic present.  The amplitude coefficients of the harmonics are
given by the exponential series \code{a}, \code{a * multiplier},
\code{a * multiplier\^{}2}, $\ldots$, \code{a * multiplier\^{}(numharms-1)}.
The value \code{a} is chosen so that the sum of the amplitudes is
\code{amp}.  \code{table} is a function table containing a cosine wave.
\begin{haskelllisting}

> genBuzz :: SigExp -> SigExp -> SigExp -> SigExp -> SigExp
>             -> SigExp -> SigExp
> genBuzz table multiplier loharm numharms amp freq =
>    sigGen "gbuzz" AR 1 [amp, freq, numharms, loharm, multiplier, table]

\end{haskelllisting}
\item \function{buzz} is a special case of \function{genBuzz} in which
\code{loharm = 1.0} and \code{multiplier = 1.0}.
\code{table} is a function table containing a sine wave:
\begin{haskelllisting}

> buzz :: SigExp -> SigExp -> SigExp -> SigExp -> SigExp
> buzz table numharms amp freq =
>    sigGen "buzz" AR 1 [amp, freq, numharms, table]

\end{haskelllisting}

Note that the above two constructors have an analog in the generating
routine \refgen{11} and the related function \function{cosineHarms}
(see \secref{function-table}).  \function{cosineHarms} stores into a table the
same waveform that would be generated by \function{buzz} or \function{genBuzz}.
However, although \function{cosineHarms} is more efficient, it has fixed
arguments and thus lacks the flexibility of \function{buzz} and
\function{genBuzz} in being able to vary the argument values with time.

\item \code{pluck table freq2 decayMethod amp freq} is an
audio signal that simulates a plucked string or drum sound,
constructed using the Karplus-Strong algorithm.  The signal has
amplitude \code{amp} and frequency \code{freq2}.  It is produced by
iterating through an internal buffer that initially contains a copy of
\code{table} and is smoothed with frequency \code{freq} to simulate the natural
decay of a plucked string.  If 0.0 is used for \code{table}, then the
initial buffer is filled with a random sequence.  There are six
possible decay modes:
\begin{enumerate}
\item \keyword{simple smoothing}, which ignores the two arguments;
\item \keyword{stretched smoothing}, which stretches the smoothing time by
a factor of \code{decarg1}, ignoring \code{decarg2};
\item \keyword{simple drum}, where \code{decarg1} is a ``roughness factor''
(0 for pitch, 1 for white noise; a value of 0.5 gives an optimal snare
drum sound);
\item \keyword{stretched drum}, which contains both roughness ({\tt
decarg1}) and stretch (\code{decarg2}) factors;
\item \keyword{weighted smoothing}, in which \code{decarg1} gives the
weight of the current sample and \code{decarg2} the weight of the
previous one (\code{decarg1+decarg2} must be $\leq1$); and
\item \keyword{recursive filter smoothing}, which ignores both arguments.
\end{enumerate}
Here again are some helpful constants:
\begin{haskelllisting}

> data PluckDecayMethod =
>      PluckSimpleSmooth
>    | PluckStretchSmooth SigExp
>    | PluckSimpleDrum SigExp
>    | PluckStretchDrum SigExp SigExp
>    | PluckWeightedSmooth SigExp SigExp
>    | PluckFilterSmooth

\end{haskelllisting}
And here is the Haskore code for the CSound pluck function:
\begin{haskelllisting}

> pluck :: SigExp -> SigExp -> PluckDecayMethod
>           -> SigExp -> SigExp -> SigExp
> pluck table freq2 decayMethod amp freq =
>    sigGen "pluck" AR 1
>           ([amp, freq, freq2, table] ++
>            case decayMethod of
>              PluckSimpleSmooth ->
>                 [constInt 1]
>              PluckStretchSmooth stretch ->
>                 [constInt 2, stretch]
>              PluckSimpleDrum roughness ->
>                 [constInt 3, roughness]
>              PluckStretchDrum roughness stretch ->
>                 [constInt 4, roughness, stretch]
>              PluckWeightedSmooth weightCur weightPrev ->
>                 [constInt 5, weightCur, weightPrev]
>              PluckFilterSmooth ->
>                 [constInt 6])

\end{haskelllisting}

\item \code{delay delayTime sig} takes a signal \code{sig} and
delays it by \code{delayTime} ---
basically making it start \code{delayTime} later than it normally would have.
This is a simple version of delay lines and delay taps, capable
of performing all of the effects that don't involve feeding the result of a
delay or a tap back into the input.
This topic is more complicated and will be considered in the next section.
In constrast to \function{delay},
the function \function{vdelay} also allows for a controlled delay.
But for memory allocation reasons
it must also know the maximum possible delay (in seconds).
\begin{haskelllisting}

> delay :: SigExp -> SigExp -> SigExp
> delay delayTime sig = sigGen "delay" AR 1 [sig, delayTime]

> vdelay :: SigExp -> SigExp -> SigExp -> SigExp
> vdelay maxDelay delayTime sig =
>    sigGen "vdelay" AR 1 [sig, delayTime, maxDelay*1000]

\end{haskelllisting}

\item Reverberation can be added to a signal using the CSound functions
\code{comb looptime revtime sig}, \code{alpass looptime revtime sig}, and
\code{reverb revtime sig}.  \code{revtime} is the time in seconds
it takes a signal to decay to 1/1000th of its original amplitude, and
\code{looptime} is the echo density.  \code{comb} produces a ``colored''
reverb, \code{alpass} a ``flat'' reverb, and \code{reverb} a ``natural
room'' reverb:
\begin{haskelllisting}

> comb :: SigExp -> SigExp -> SigExp -> SigExp
> comb looptime revtime sig =
>    sigGen "comb" AR 1 [sig, revtime, looptime]

> alpass :: SigExp -> SigExp -> SigExp -> SigExp
> alpass looptime revtime sig =
>    sigGen "alpass" AR 1 [sig, revtime, looptime]

> reverb :: SigExp -> SigExp -> SigExp
> reverb revtime sig =
>    sigGen "reverb" AR 1 [sig, revtime]

\end{haskelllisting}
\end{enumerate}

\subparagraph{Delay Lines and Tapping}

\code{DelayLine deltime audiosig} establishes a digital delay line,
where \code{audiosig} is the source, and \code{deltime} is the delay
time in seconds.  That \code{DelayLine} can either be simply read,
by the \code{Result delayline} constructor, or tapped, by the
\code{Tap tapname delayline args} constructor.

The most common tap functions are \csoundfunc{deltap} and \csoundfunc{deltapi,}
where \csoundfunc{deltapi} is the interpolating version of \csoundfunc{deltap}.
Thus we will include helper functions for both of those functions:
\begin{haskelllisting}

> delTap, delTapI :: DelayLine -> SigExp -> SigExp
> delTap  dl tap = TreeRec.Branch (Tap "deltap"  dl [tap])
> delTapI dl tap = TreeRec.Branch (Tap "deltapi" dl [tap])

\end{haskelllisting}

\subparagraph{Recursive Statements}

In some cases, the user may want their instrument to have certain
special effects --- such as an infinite echo, going back and forth
but getting fainter and fainter.  It would seem logical that the user
would, in that case, write something like this:
\begin{haskelllisting}

  x = sig + delay (0.5 * x) 1.0

\end{haskelllisting}

Unfortunately, the translation process cannot handle statements like
that, and any kind of statement which is defined in terms of itself
must be written a different way.  {\em Within} Haskore, recursive
statements are handled using three constructors: \constructor{Loop}, \constructor{Var},
and \constructor{Rec}.  However, these three constructors are not available
to the users, and so we offer a very simple solution: the \function{rec}
function:
\begin{haskelllisting}

> rec :: (SigExp -> SigExp) -> SigExp
> rec = TreeRec.recourse

\end{haskelllisting}

In order to perform the infinite echo listed above, we would write this
code:
\begin{haskelllisting}

  x = rec (\y -> sig + delay (0.5 * y) 1.0)

\end{haskelllisting}
Thus \function{rec}, in some ways, is a bit like \function{fix}, although it doesn't
actually do the computation --- instead, it juggles some code around and
passes the problem off to CSound.

When the \type{SigExp} is processed, all \constructor{Rec} constructors are
converted into a \type{SigExp} with \constructor{Loop} and \constructor{Var}
constructors.  Each \constructor{Loop} has some number of matching \constructor{Var}
statements, with the same unique integer referring to both.  This is
done through the \function{runFix} function and its various helper functions:

\begin{haskelllisting}

> type SigFixed = TreeTag.T TreeRec.Tag SigTerm
>
> runFix, simpleFix :: SigExp -> SigFixed
> runFix = addEqTree . TreeRec.toTaggedUnique 1
> {- some expressions need no loop unwinding,
>    toTagged does unwinding anyway, but with less overhead
>    and shared loop ids -}
> simpleFix = TreeRec.toTagged 0
>
> instance Foldable BooleanTerm where
>    foldMap = Traversable.foldMapDefault
>
> instance Traversable BooleanTerm where
>    sequenceA branch =
>       case branch of
>         Operator   nm left right ->
>            liftA2 (Operator   nm) (sequenceA left) (sequenceA right)
>         Comparison nm left right ->
>            liftA2 (Comparison nm) (          left) (          right)
>
> instance Foldable DelayLineTerm where
>    foldMap = Traversable.foldMapDefault
>
> instance Traversable DelayLineTerm where
>    sequenceA (DelayLine x y) = liftA2 DelayLine x y
>
> instance Foldable SigTerm where
>    foldMap = Traversable.foldMapDefault
>
> instance Traversable SigTerm where
>    sequenceA branch =
>      case branch of
>         {- compare with Functor instance -}
>         ConstFloat x  -> pure $ ConstFloat x
>         ConstInt n    -> pure $ ConstInt n
>         TableNumber t -> pure $ TableNumber t
>         PField n      -> pure $ PField n
>         Str str       -> pure $ Str str
>         Read t        -> pure $ Read t
>         Tap nm del xs -> liftA2 (Tap nm) (sequenceA del) (sequenceA xs)
>         Result del    -> liftA  Result   (sequenceA del)
>         Conditional b true false ->
>            liftA3 Conditional (sequenceA b) true false
>         Infix  nm left right    -> liftA2 (Infix nm)  left right
>         Prefix nm arg           -> liftA  (Prefix nm) arg
>         SigGen nm rate cnt args ->
>            liftA (SigGen nm rate cnt) (sequenceA args)
>         Index cnt x -> liftA (Index cnt) (sequenceA x)

> -- fixSig (Rec (LoopFunction f)) =
> --    do n <- get; put (n + 1); fixSig (Loop n (addEq (f (Var n))))

> addEqTree :: SigFixed -> SigFixed
> addEqTree (TreeTag.Branch x)  = TreeTag.Branch (fmap addEqTree x)
> addEqTree (TreeTag.Tag t x) = TreeTag.Tag t (addEqTree (addEq x))
> addEqTree (TreeTag.Loop t)  = TreeTag.Loop t

> addEq :: SigFixed -> SigFixed
> addEq ex =
>    case ex of
>       TreeTag.Branch (SigGen _ _ _ _) -> ex
>       TreeTag.Branch (Tap _ _ _)      -> ex
>       TreeTag.Branch (Result _)       -> ex
>       _ -> TreeTag.Branch (SigGen "="
>               (if CR == getRate ex
>                  then CR else AR) 1 [ex])

> getRate :: SigFixed -> EvalRate
> getRate (TreeTag.Branch branch) = getRateTerm branch
> getRate (TreeTag.Tag _ arg) = getRate arg
> getRate (TreeTag.Loop _) = error "getRate: undefined rate"

> getRateTerm :: SigTerm SigFixed -> EvalRate
> getRateTerm branch =
>    case branch of
>       Tap _ _ _         -> AR
>       Result _          -> AR
>       Conditional _ a b -> max (getRate a) (getRate b)
>       Infix _ a b       -> max (getRate a) (getRate b)
>       Prefix _ arg      -> getRate arg
>       SigGen _ rt _ _   -> rt
>       Index _ arg       -> getRateTerm arg
>       _                 -> NR

\end{haskelllisting}

Note that the \function{addEq} function is used to add an equal sign to the
statement being looped, provided that the statement is not already one
of the signal generating ones.  Also note that if the rate of the
statement is \constructor{NR}, the new rate will be \constructor{AR} --- this is because
you cannot have an infinitely recursive statement at the note rate.

Ideally, all \type{SigExp} statements should have \function{runFix} applied
to them.  So we have the \function{getFixedExpressions} function, used as
a replacement to the standard \function{getChannels} of the \type{Output} class:
\begin{haskelllisting}

> getFixedExpressions :: Output a => a -> [SigFixed]
> getFixedExpressions = map (aux . runFix) . getChannels
>    where aux ex =
>             if AR == getRate ex
>               then ex
>               else TreeTag.Branch (SigGen "=" AR 1 [ex])

\end{haskelllisting}

\subparagraph{Signal Generators with Multiple Outputs}

When looking through the CSound documentation, you may notice that there are
certain functions, such as \csoundfunc{convolve} or \csoundfunc{babo} that do not have the same
structure in CSound as the most of the rest of the functions.  This is because
those are two operators that actually return multiple outputs.  While this type
of function is not extremely common, we have included code that can, in fact,
handle such functions.  The third argument to the \function{sigGen} constructor
actually specifies the number of arguments to be returned.  In most cases, this
should simply be set to one; in a few cases, such as \csoundfunc{convolve} or \csoundfunc{babo},
this should be set to however many outputs you want returned from the function.

But how do you get to those outputs?  Well, the \constructor{Index} constructor is
used from within the code, but the user cannot access that.  So we have the
following function:

\begin{haskelllisting}

> getMultipleOutputs :: SigExp -> [SigExp]
> getMultipleOutputs (TreeRec.Branch ex@(SigGen _ _ outCount _)) =
>    if outCount==1
>      then error ("cannot get multiple outputs from a function with one output")
>      else map (TreeRec.Branch . flip Index ex) [1..outCount]
> getMultipleOutputs _ =
>    error ("cannot get multiple outputs from a non-SigGen")

\end{haskelllisting}

Which can be called on any \function{sigGen} statement returning multiple
arguments, and returns a list of the outputs.  In other words, you could
write something like this:
\begin{haskelllisting}

  [a1, a2] = getMultipleOutputs
                (LineStatement "babo" AR 2 [sig, 0, 0, 0, 5, 5, 5])

\end{haskelllisting}

Haskell would then pattern-match, and leave you with two variables,
\code{a1} and \code{a2}.

\paragraph{Output Operators}

Now that we've got all of those interesting methods of signal generation
under our belts, we need some way to make CSound play these interesting
sound waves.  Hence, the \keyword{output statements}, all of which must be
instances of the {\tt Output} class:
\begin{haskelllisting}

> class (Show c, Eq c) => Output c where
>    getChannels :: c -> [SigExp]
>    getName :: c -> String
>    getChannelCount :: c -> Int

\end{haskelllisting}

The \function{getChannelCount} could be pre-defined
with \code{length . getChannels}
but this would require that we have actually an \type{Output} value at hand
when calling \function{getChannelCount}.

We have defined several common types of output, including
\type{Mono}, which allows for the writing of one output channel; \type{Stereo},
which allows for two; and \type{Quad}, which, unsurprisingly, allows four:
\begin{haskelllisting}

> data Mono   = Mono   SigExp deriving (Show, Eq)
> data Stereo = Stereo SigExp SigExp deriving (Show, Eq)
> data Quad   = Quad   SigExp SigExp SigExp SigExp deriving (Show, Eq)
>
> instance Output Mono where
>    getChannels (Mono x) = [x]
>    getName _ = "out"
>    getChannelCount _ = 1
>
> instance Output Stereo where
>    getChannels (Stereo x1 x2) = [x1, x2]
>    getName _ = "outs"
>    getChannelCount _ = 2
>
> instance Output Quad where
>    getChannels (Quad x1 x2 x3 x4) = [x1, x2, x3, x4]
>    getName _ = "outq"
>    getChannelCount _ = 4

\end{haskelllisting}

The user is welcome to add more by declaring them instances of the
{\tt Output} class and then filling out the required methods.

\paragraph{Converting Orchestra Values to Orchestra Files}

We must now convert the \type{SigExp} values into a form which can be
written into a CSound {\tt .sco} file.  As mentioned earlier, each
signal generation or modification statement in CSound assigns its
result a string name.  This name is used whenever another statement
takes the signal as an argument.  Names of signals generated at note
rate must begin with the letter \csoundfunc{i}, control rate with letter \csoundfunc{k},
and audio rate with letter \csoundfunc{a}.  The output statements do not
generate a signal so they do not have a result name.

\begin{figure}
{\scriptsize\vspace{-.9in}
\begin{haskelllisting}

> mkList :: SigFixed -> [SigFixed]
> mkList ex@(TreeTag.Branch n)  = ex : mkListTerm n
> mkList ex@(TreeTag.Tag _ x) = ex : mkList x
> mkList    (TreeTag.Loop _)  = []

> mkListTerm :: SigTerm SigFixed -> [SigFixed]
> mkListTerm term =
>    case term of
>      Tap _ dl lst      -> mkListDL dl ++ mkListAll lst
>      Result dl         -> mkListDL dl
>      Conditional a b c -> mkListBool a ++ mkListAll [b, c]
>      Infix _ a b       -> mkListAll [a, b]
>      Prefix _ x        -> mkList x
>      SigGen _ _ outCount lst ->
>         if outCount == 1
>           then mkListAll lst
>           else map (TreeTag.Branch . flip Index term) [1..outCount]
>                  ++ mkListAll lst
>                -- cf. getMultipleOutputs
>      Index _ expr      -> mkListTerm expr
>      _                 -> []

\end{haskelllisting}
}
\caption{The \function{mkList} Function}
\figlabel{mkList}
\end{figure}

The function \function{mkList} is shown in \figref{mkList}, and
generates a list containing every single sub-expression of the given
\type{SigExp}.  It uses the following auxiliary functions:
\begin{haskelllisting}

> type DelayLineFixed = DelayLineTerm SigFixed
> type BooleanFixed   = BooleanTerm SigFixed

> mkListAll :: [SigFixed] -> [SigFixed]
> mkListAll = concatMap mkList

> mkListDL :: DelayLineFixed -> [SigFixed]
> mkListDL (DelayLine x1 x2) = mkListAll [x1, x2]

> mkListBool :: BooleanFixed -> [SigFixed]
> mkListBool (Operator   _ a b) = concatMap mkListBool [a, b]
> mkListBool (Comparison _ a b) = mkListAll [a, b]

> mkListOut :: Output a => InstrBlock a -> [SigFixed]
> mkListOut (InstrBlock _ xtim chnls lst) =
>    mkListAll (simpleFix xtim : getFixedExpressions chnls ++
>                  map (simpleFix . snd) lst)
>               -- there should not be any loop to be unwind in lst

\end{haskelllisting}

Once we have the list of all of the expressions, we need to find the
signal-generating ones, like \constructor{Tap}s and \function{sigGen}s, and convert
them into a list of \type{StatementDef}s, with their associated rates.
This is done using the function \function{getLineRates}.

\begin{haskelllisting}

> type LineFunctionRates = [(EvalRate, StatementDef)]

> data StatementDef = StatementDef Function [SigFixed]
>                   | TapDef Function DelayLineFixed [SigFixed]
>                   | DelayDef DelayLineFixed
>                   | DelayWriteDef DelayLineFixed
>                   | MultiDef Function [SigFixed]
>                              OutCount (SigTerm SigFixed)
>                   | IndexDef OutCount (SigTerm SigFixed)
>      deriving (Show, Eq)

> getLineRates :: [SigFixed] -> LineFunctionRates
> getLineRates = mapMaybe aux
>    where
>       aux (TreeTag.Branch n) =
>          case n of
>            Tap nm dl lst       -> Just (AR, TapDef nm dl lst)
>            Result dl           -> Just (AR, DelayDef dl)
>            SigGen nm rt ct lst -> Just (rt,
>                                     if ct==1
>                                       then StatementDef nm lst
>                                       else MultiDef nm lst ct n)
>            Index ct ex@(SigGen _ rt _ _) ->
>                                   Just (rt, IndexDef ct ex)
>            _                   -> Nothing
>       aux _ = Nothing

\end{haskelllisting}

\type{DelayLine}s and \type{Tap}s are a rather complex problem in Haskore.
In CSound, there is no such thing as an explicit delay line; you establish
a delay line with a \csoundfunc{delayr} opcode, and then all taps that occur between
that line and the matching \csoundfunc{delayw} line belong to that particular delay
line.  Thus the translation from the Haskore concept of delay lines to the
CSound concept is somewhat difficult.  Hence \function{procDelay} and its various
helper functions, which gather all of the taps together and add the requisite
\type{DelayWriteDef} to the end of them:

\begin{haskelllisting}

> procDelay :: LineFunctionRates -> LineFunctionRates
> procDelay lst@((_, DelayDef dl) : _)   = setUpDelays lst dl
> procDelay lst@((_, TapDef _ dl _) : _) = setUpDelays lst dl
> procDelay (hd : tl)                    = hd : procDelay tl
> procDelay []                           = []

> setUpDelays :: LineFunctionRates -> DelayLineFixed -> LineFunctionRates
> setUpDelays lst dl =
>    let aux (_, DelayDef dl2)   = dl == dl2
>        aux (_, TapDef _ dl2 _) = dl == dl2
>        aux _                   = False
>        (dels, rest) = partition aux lst
>     in procTaps dels dl ++ procDelay rest

> procTaps :: LineFunctionRates -> DelayLineFixed -> LineFunctionRates
> procTaps lst dl =
>    [(AR, DelayDef dl)] ++ filter aux lst ++ [(AR, DelayWriteDef dl)]
>    where aux (_, TapDef _ _ _) = True
>          aux _                 = False

\end{haskelllisting}

Putting all of the above together, here is a function that converts an
\type{SigExp} into a list of proper name / \type{StatementDef} pairs.  Each
one of these will eventually result in one statement in the CSound
orchestra file.  (The result of \function{getLineRates} is reversed to ensure
that a definition exists before it is used; and this must be done {\em
before} \function{nub} is applied (which removes duplicates), for the same
reason.)
\begin{haskelllisting}

> type StatementDefs = [(Name, StatementDef)]

> extractFunctions :: [SigFixed] -> StatementDefs
> extractFunctions =
>    zipWith giveName [1 ..] . nub . procDelay . reverse . getLineRates

> giveName :: Int -> (EvalRate, StatementDef) -> (Name, StatementDef)
> giveName n (er,x) =
>    let var = case er of
>                AR -> 'a'
>                CR -> 'k'
>                NR -> 'i'
>    in (var : show n, x)

\end{haskelllisting}

The functions that follow are used to write the orchestra file.
\function{saveIA} is similar to \function{Score.saveIA}:
it asks the user for a file name, opens the file,
writes the given orchestra value to the file, and then closes the file.
\begin{haskelllisting}

> saveIA :: Output a => T a -> IO ()
> saveIA orch =
>    do putStr "\nName your orchestra file "
>       putStr "(.orc extension will be added): "
>       name <- getLine
>       save name orch

> save :: Output a => FilePath -> T a -> IO ()
> save name orch =
>    writeFile (name ++ ".orc") (toString orch)

\end{haskelllisting}

\function{CSound.Orchestra.toString} splits the task of writing the
orchestra into two parts: writing the header, and writing the instrument
blocks.
\begin{haskelllisting}

> toString :: Output a => T a -> String
> toString orc@(Cons hdr ibs) =
>    let glob = getGlobal ibs
>    in  unlines $
>        headerToString hdr (channelCount orc) ++
>        maybe [] writeGlobalHeader glob ++
>        concatMap instrBlockToString ibs ++
>        maybe [] resetGlobals glob

\end{haskelllisting}
Writing the header is relatively simple, and is accomplished by the
following function:
\begin{haskelllisting}

> headerToString :: Header -> Int -> [String]
> headerToString (a,k) nc =
>   ["sr     = " ++ show a,
>    "kr     = " ++ show k,
>    "ksmps  = " ++ show (fromIntegral a / fromIntegral k :: Double),
>    "nchnls = " ++ show nc]

> channelCount :: Output a => T a -> Int
> channelCount (Cons _ instrBlock) =
>    getChannelCount (instrBlockOutput (head instrBlock))

\end{haskelllisting}

If the instance of \function{getChannelCount}
does not rely on \function{getChannels}
the \expression{instrBlock} can be empty.


\function{instrBlockToString} writes a single instrument block.
\begin{haskelllisting}

> instrBlockToString :: Output a => InstrBlock a -> [String]
> instrBlockToString ib@(InstrBlock num xtim _ _) =
>    let ses = mkListOut ib
>        noes = extractFunctions ses
>        lps = getLoops noes ses
>     in "" :
>        showInstrument num :
>        writeLoops lps ++
>        concatMap (writeExp noes lps) noes ++
>        writeOut noes lps ib ++
>        (if xtim /= 0
>          then ["xtratim " ++ showExp noes lps (simpleFix xtim)]
>          else []) ++
>        "endin" :
>        []

> showInstrument :: Instrument ->  String
> showInstrument instr = "instr " ++ showInstrumentNumber instr

\end{haskelllisting}

\constructor{Loop} statements require special handling, including initialization at
the top of each instrument and a special set of loop definitions which are
also passed to most of the writing functions.  This is handled by the
following two functions:
\begin{haskelllisting}

> type LoopDefs = [(TreeRec.Tag, String)]

> writeLoops :: LoopDefs -> [String]
> writeLoops = map ((++ " init 0") . snd)

> getLoops :: StatementDefs -> [SigFixed] -> LoopDefs
> getLoops noes =
>    let extractTag (TreeTag.Tag n ex) = Just (n, ex)
>        extractTag _                  = Nothing
>    in  map (mapSnd (showExp noes []))
>              . nub . mapMaybe extractTag
>      -- map and mapMaybe are separated for efficiency achieved by nub

\end{haskelllisting}

Globals, too, require special handling: they need both a header at the top
of the CSound orchestra file, and an instrument in which to reset their values.
Those requirements are fulfilled by the following functions, which are called
from the \function{instrBlockToString} function.

\begin{haskelllisting}

> globalRate :: EvalRate -> String
> globalRate AR = "a"
> globalRate CR = "k"
> globalRate NR = error ("you cannot use init-rate globals")

> globalWrite, globalRead :: GlobalSig -> String
> globalWrite (Global rate _ n) = "g" ++ globalRate rate ++ "w" ++ show n
> globalRead  (Global rate _ n) = "g" ++ globalRate rate ++ "r" ++ show n

> resetGlobals :: ([GlobalSig], Instrument) -> [String]
> resetGlobals (gs,num) =
>    let aux g =
>           (globalRead g ++ " = " ++ globalWrite g) :
>           (globalWrite g ++ " = 0") :
>           []
>    in  "" :
>        showInstrument num :
>        concatMap aux gs ++
>        "endin" :
>        []

> numGlobalInstrs :: Output a => [InstrBlock a] -> Instrument
> numGlobalInstrs lst =
>    head (instruments \\ map instrBlockInstr lst)

> getGlobals :: Output a => [InstrBlock a] -> [GlobalSig]
> getGlobals = concatMap (map fst . instrBlockGlobals)

> getGlobal :: Output a => [InstrBlock a] -> Maybe ([GlobalSig], Instrument)
> getGlobal lst =
>    let gs = getGlobals lst
>    in  toMaybe (not (null gs)) (gs, numGlobalInstrs lst)

> writeGlobalHeader :: ([GlobalSig], Instrument) -> [String]
> writeGlobalHeader (gs,num) =
>    let globInit g =
>           (globalWrite g ++ " init 0") :
>           (globalRead  g ++ " init 0") :
>           []
>        contents =
>           concatMap globInit gs ++
>           ("turnon " ++ showInstrumentNumber num) :
>           []
>    in  "" : contents ++ "" : []

> writeOutGlobals :: StatementDefs -> LoopDefs ->
>                       [(GlobalSig, SigFixed)] -> [String]
> writeOutGlobals noes lps =
>    let aux (g, oe) =
>           globalWrite g ++ " = " ++ globalWrite g ++ " + " ++
>           writeArgs noes lps [oe]
>    in  map aux

\end{haskelllisting}

Recall that after processing, the \type{SigExp} becomes a list of
\code{(Name, StatementDef)} pairs.  The last few functions write each of these
named \type{StatementDef}s as a statement in the orchestra file.  Whenever a
signal generation/modification constructor is encountered in an
argument list of another constructor, the argument's string name is
used instead, as found in the list of \type{(Name, StatementDef)} pairs.

\begin{figure}
{\small
\begin{haskelllisting}

> writeOut :: Output a => StatementDefs -> LoopDefs -> InstrBlock a -> [String]
> writeOut noes lps (InstrBlock _ _ chnls lst) =
>    (getName chnls ++ " " ++ writeArgs noes lps (getFixedExpressions chnls)) :
>    writeOutGlobals noes lps (map (mapSnd simpleFix) lst)

> writeExp :: StatementDefs -> LoopDefs -> (Name, StatementDef) -> [String]
> writeExp noes lps (name, stmt) =
>    case stmt of
>      StatementDef funcName args ->
>         [ifAllowedArgs funcName args
>           (name ++ " " ++ funcName ++ " " ++ writeArgs noes lps args)]
>      DelayDef (DelayLine _ del) ->
>         [name ++ " delayr " ++ showExp noes lps del]
>      TapDef funcName _ args ->
>         [ifAllowedArgs funcName args
>           (name ++ " " ++ funcName ++ " " ++ writeArgs noes lps args)]
>      DelayWriteDef (DelayLine sig _) ->
>         ["delayw " ++ showExp noes lps sig]
>      IndexDef _ _ -> []
>      MultiDef funcName args outCount ex {- 'ex' is always a SigGen -} ->
>         [ifAllowedArgs funcName args
>            (concat (intersperse ", "
>               (map (\x -> showExp noes lps
>                             (TreeTag.Branch (Index x ex)))
>                    [1..outCount]))
>              ++ " " ++ funcName ++ " " ++ writeArgs noes lps args)]
>
> ifAllowedArgs :: String -> [SigFixed] -> String -> String
> ifAllowedArgs funcName args str =
>    if allowedArgs argCountTable funcName (length args)
>      then str
>      else error ("writeExp: wrong number of arguments " ++
>                            "passed to function " ++ funcName)

> writeArgs :: StatementDefs -> LoopDefs -> [SigFixed] -> String
> writeArgs noes lps =
>    concat . intersperse ", " . map (showExp noes lps)

\end{haskelllisting}
}
\caption{The Function \function{writeExp}}
\figlabel{writeExp}
\end{figure}

\begin{figure}
{\small
\begin{haskelllisting}

> showExp :: StatementDefs -> LoopDefs -> SigFixed -> String
> showExp noes lps (TreeTag.Branch oe) =
>    case oe of
>      ConstFloat x  -> show x
>      ConstInt n    -> show n
>      TableNumber n -> show n
>      PField p      -> "p" ++ show p
>      Str s         -> show s
>      Read var      -> globalRead var
>      Conditional b tr fa ->
>         "(" ++ showBool noes lps b ++ " ? "
>             ++ showExp noes lps tr ++ " : "
>             ++ showExp noes lps fa ++ ")"
>      Infix nm x1 x2 ->
>         "(" ++ showExp noes lps x1 ++ " " ++ nm ++ " "
>             ++ showExp noes lps x2 ++ ")"
>      Prefix nm x -> nm ++ "(" ++ showExp noes lps x ++ ")"
>      SigGen nm _ _ args ->
>         lookupDef noes (StatementDef nm args) oe
>      Result dl      -> lookupDef noes (DelayDef dl) oe
>      Tap nm dl args -> lookupDef noes (TapDef nm dl args) oe
>      Index x ex -> lookupDef noes (IndexDef x ex) oe
> showExp noes lps (TreeTag.Tag _ ex) =
>    showExp noes lps ex
> showExp _    lps (TreeTag.Loop s) =
>    maybe (error "loop not found") id (lookup s lps)

> lookupDef :: (Show a, Eq c) => [(b, c)] -> c -> a -> b
> lookupDef noes def oe =
>    maybe (error ("showExp " ++ show oe ++ ": constructor not found\n"))
>          id (lookup def (map (\(x, y) -> (y, x)) noes))

> showBool :: StatementDefs -> LoopDefs -> BooleanFixed -> String
> showBool noes lps bool =
>    case bool of
>       Operator name x1 x2 ->
>          "(" ++ showBool noes lps x1 ++ " " ++ name ++ " "
>              ++ showBool noes lps x2 ++ ")"
>       Comparison name x1 x2 ->
>          "(" ++ showExp noes lps x1 ++ " " ++ name ++ " "
>              ++ showExp noes lps x2 ++ ")"

\end{haskelllisting}
}
\caption{The Function \function{showExp}}
\figlabel{showExp}
\end{figure}

\paragraph{The \type{Orc} Monad}

The global signals can be somewhat difficult to handle, especially when there
are quite a few of them.  After all, they must all be different; otherwise,
the user may have two instruments writing completely different things to the
same signal, and using the same signals for completely different things.  However,
there is an easier way to do this --- a monad that allows for a much simpler way
of getting global signals:

\begin{haskelllisting}

> type Orc a b = State (OrcState a) b
> data OrcState a = OrcState [InstrBlock a] Int deriving (Show, Eq)

> mkSignalPlain :: EvalRate -> (SigExp -> SigExp -> SigExp) -> OrcState a
>                     -> (GlobalSig, OrcState a)
> mkSignalPlain rate func (OrcState ibs gCount) =
>    (Global rate func gCount, OrcState ibs (gCount + 1))

> mkSignal :: Output a => EvalRate -> (SigExp -> SigExp -> SigExp)
>                -> Orc a GlobalSig
> mkSignal rate func = state (mkSignalPlain rate func)

> addInstrPlain :: Output a => InstrBlock a -> OrcState a -> OrcState a
> addInstrPlain ib (OrcState ibs gCount) =
>    OrcState (ibs ++ [ib]) gCount

> addInstr :: Output a => InstrBlock a -> Orc a ()
> addInstr ib = modify (addInstrPlain ib)

> runOrc :: Orc a () -> [InstrBlock a]
> runOrc comp =
>    case execState comp (OrcState [] 1) of
>       (OrcState ibs _) -> ibs

> mkOrc :: Output a => Header -> Orc a () -> T a
> mkOrc hdr = Cons hdr . runOrc

\end{haskelllisting}

The user can call \function{mkSignal} to get a unique global line, or
\function{addInstr} to add an instrument to the structure.  For example:

\begin{haskelllisting}

> test :: IO ()
> test =
>    let a1 = oscI AR (tableNumber 1) 1000 440
>        comp =
>           do h <- mkSignal AR (+)
>              addInstr (InstrBlock (instrument 1) 0 (Mono a1) [(h, a1)])
>              addInstr (InstrBlock (instrument 2) 0 (Mono (readGlobal h)) [])
>    in  saveIA (mkOrc (44100, 4410) comp)

\end{haskelllisting}

The above example has the first instrument writing a simple oscillation
to the given audio-rate global signal, and then has the second instrument
reading from the same global.

\paragraph{An Orchestra Example}

\figref{csound-orc-file} shows a typical CSound orchestra
file.  \figref{orc-def} shows how this same functionality
would be achieved in Haskore using an \type{CSound.Orchestra.T} value.  Finally,
\figref{orc-file-result} shows the result of applying
\function{Orchestra.saveIA} to \code{orc1} shown in \figref{orc-def}.
Figures \ref{fig:csound-orc-file} and \ref{fig:orc-file-result}
should be compared: you will note that except for name changes, they
are the same, as they should be.

\begin{figure}
\begin{verbatim}

sr = 48000
kr = 24000
ksmps = 2
nchnls = 2

instr 4

inote = cpspch(p5)

k1 envlpx ampdb(p4), .001, p3, .05, 6, -.1, .01
k2 envlpx ampdb(p4), .0005, .1, .1, 6, -.05, .01
k3 envlpx ampdb(p4), .001, p3, p3, 6, -.3, .01

a1 oscili k1, inote, 1
a2 oscili k1, inote * 1.004, 1
a3 oscili k2, inote * 16, 1
a4 oscili k3, inote, 5
a5 oscili k3, inote * 1.004, 5

outs  (a2 + a3 + a4) * .75, (a1 + a3 + a5) * .75

endin

\end{verbatim}
\caption{Sample CSound Orchestra File}
\figlabel{csound-orc-file}
\end{figure}

\begin{figure}
\begin{haskelllisting}

> orc1 :: T Stereo
> orc1 =
>   let hdr   = (48000, 24000)
>       inote = pchToHz p5
>       k1    = env  CR 6 (-0.1)  0.01 0 0.05 0.001  p3  (dbToAmp p4)
>       k2    = env  CR 6 (-0.05) 0.01 0 0.1  0.0005 0.1 (dbToAmp p4)
>       k3    = env  CR 6 (-0.3)  0.01 0 p3   0.001  p3  (dbToAmp p4)
>       t1    = tableNumber 1
>       t5    = tableNumber 5
>       a1    = oscI AR t1 k1  inote
>       a2    = oscI AR t1 k1 (inote*1.004)
>       a3    = oscI AR t1 k2 (inote*16)
>       a4    = oscI AR t5 k3  inote
>       a5    = oscI AR t5 k3 (inote*1.004)
>       out   = Stereo ((a2+a3+a4) * 0.75) ((a1+a3+a5) * 0.75)
>       ib    = InstrBlock (instrument 4) 0 out []
>   in Cons hdr [ib]

> test1 :: StatementDefs
> test1 = extractFunctions $ mkListOut (head ((\(Cons _ x) -> x) orc1))

\end{haskelllisting}
\caption{Haskore Orchestra Definition}
\figlabel{orc-def}
\end{figure}

\begin{figure}
\begin{verbatim}

sr     = 48000
kr     = 24000
ksmps  = 2.0
nchnls = 2

instr 4
k1 envlpx ampdb(p4), 1.0e-3, p3, p3, 6.0, -(0.3), 1.0e-2, 0.0
a2 oscili k1, (cpspch(p5) * 1.004), 5
k3 envlpx ampdb(p4), 5.0e-4, 0.1, 0.1, 6.0, -(5.0e-2), 1.0e-2, 0.0
a4 oscili k3, (cpspch(p5) * 16.0), 1
k5 envlpx ampdb(p4), 1.0e-3, p3, 5.0e-2, 6.0, -(0.1), 1.0e-2, 0.0
a6 oscili k5, cpspch(p5), 1
a7 oscili k1, cpspch(p5), 5
a8 oscili k5, (cpspch(p5) * 1.004), 1
outs (((a8 + a4) + a7) * 0.75), (((a6 + a4) + a2) * 0.75)
endin

\end{verbatim}
\caption{Result of \code{Orchestra.saveIA orc1}}
\figlabel{orc-file-result}
\end{figure}
