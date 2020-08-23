\subsubsection{Equivalence of Literal Performances}
\seclabel{equivalence}

\newcommand\equivalent{$\ \ \equiv\ \ $}

A \keyword{literal performance} is one in which no aesthetic
interpretation is given to a musical object.
The function \function{Pf.fromMusic} in fact yields a literal performance;
aesthetic nuances must be expressed explicitly using note and phrase attributes.

There are many musical objects whose literal performances we expect to
be \keyword{equivalent}.  For example, the following two musical objects
are certainly not equal as data structures, but we would expect their
literal performances to be identical:
\begin{center}
\code{(m0 +:+\ m1) +:+\ (m2 +:+\ m3)} \\
\code{m0 +:+\ m1 +:+\ m2 +:+\ m3}
\end{center}
Thus we define a notion of equivalence:

\begin{definition}
Two musical objects \code{m0} and \code{m1} are \keyword{equivalent},
written \code{m0}$\ \equiv\ $\code{m1}, if and only if:
\begin{center}
($\forall$\code{imap,c})\quad
\code{Pf.fromMusic imap c m0 = Pf.fromMusic imap c m1}
\end{center}
where ``\code{=}'' is equality on values
(which in Haskell is defined by the underlying equational logic).
\end{definition}

One of the most useful things we can do with this notion of
equivalence is establish the validity of certain \keyword{transformations}
on musical objects.  A transformation is {\em valid} if the result of
the transformation is equivalent (in the sense defined above) to the
original musical object; i.e.\ it is ``meaning preserving''.
Some of these connections are used in the \module{Optimization}
(\secref{optimization}) in order to simplify a musical data structure.

The most basic of these transformation we treat as \keyword{axioms} in an
\keyword{algebra of music}.  For example:

\begin{axiom}
For any \code{r0}, \code{r1}, and \code{m}:
\begin{center}
\code{changeTempo r0 (changeTempo r1 m)} \equivalent \code{changeTempo (r0*r1) m}
\end{center}
\end{axiom}

To prove this axiom, we use conventional equational reasoning
(for clarity we omit \code{imap},
simplify the context to just \code{dt},
and omit \code{fromRational}):
\begin{proof}
\begin{haskellblock}
Pf.fromMusic dt (changeTempo r0 (changeTempo r1 m))
= Pf.fromMusic (dt / r0) (changeTempo r1 m)   -- unfolding Pf.fromMusic
= Pf.fromMusic ((dt / r0) / r1) m             -- unfolding Pf.fromMusic
= Pf.fromMusic (dt / (r0 * r1)) m             -- simple arithmetic
= Pf.fromMusic dt (changeTempo (r0*r1) m)     -- folding Pf.fromMusic
\end{haskellblock}
\end{proof}

Here is another useful transformation and its validity proof (for
clarity in the proof we omit \code{imap} and simplify the context to
just \code{(t,dt)}):

\begin{axiom}
For any \code{r}, \code{m0}, and \code{m1}:
\begin{center}
\code{changeTempo r (m0 +:+\ m1)} \equivalent \code{changeTempo r m0 +:+\ changeTempo r m1}
\end{center}
\end{axiom}
In other words, {\em tempo scaling distributes over sequential composition}.
\begin{proof}
\begin{haskellblock}
Pf.fromMusic (t,dt) (changeTempo r (m0 +:+ m1))
= Pf.fromMusic (t,dt/r) (m0 +:+ m1)           -- unfolding Pf.fromMusic
= Pf.fromMusic (t,dt/r) m0 ++
     Pf.fromMusic (t',dt/r) m1                -- unfolding Pf.fromMusic
= Pf.fromMusic (t,dt) (changeTempo r m0) ++
     Pf.fromMusic (t',dt) (changeTempo r m1)  -- folding Pf.fromMusic
              where t'  = t + dur m0 * dt/r
= Pf.fromMusic (t,dt) (changeTempo r m0) ++
     Pf.fromMusic (t'',dt) (changeTempo r m1) -- folding dur
              where t'' = t + dur (changeTempo r m0) * dt
= Pf.fromMusic (t,dt)
     (changeTempo r m0 +:+ changeTempo r m1)  -- folding Pf.fromMusic
\end{haskellblock}
\end{proof}

An even simpler axiom is given by:

\begin{axiom}
For any \code{m}:
\begin{center}
\code{changeTempo 1 m} \equivalent \code{m}
\end{center}
\end{axiom}
In other words, {\em unit tempo scaling is the identity}.
\begin{proof}
\begin{haskellblock}
Pf.fromMusic (t,dt) (changeTempo 1 m)
= Pf.fromMusic (t,dt/1) m                     -- unfolding Pf.fromMusic
= Pf.fromMusic (t,dt) m                       -- simple arithmetic
\end{haskellblock}
\end{proof}

Note that the above proofs, being used to establish axioms, all
involve the definition of \function{Pf.fromMusic}.  In contrast, we can also
establish {\em theorems} whose proofs involve only the axioms.  For
example, Axioms 1, 2, and 3 are all needed to prove the following:
\begin{theorem}
For any \code{r}, \code{m0}, and \code{m1}:
\begin{center}
\code{changeTempo r m0 +:+\ m1} \equivalent \code{changeTempo r (m0 +:+\ changeTempo (recip r) m1)}
\end{center}

\begin{comment}

% propTempoPartialSerial ::
%    Dur.Ratio -> MidiMusic.T -> MidiMusic.T -> Property
% propTempoPartialSerial r m0 m1 =
%    r > 0   ==>
%       changeTempo r m0 +:+ m1 =?=
%       changeTempo r (m0 +:+ changeTempo (recip r) m1)

\end{comment}

\end{theorem}
\begin{proof}
\begin{haskellblock}
changeTempo r (m0 +:+ changeTempo (recip r) m1)
= changeTempo r m0 +:+ changeTempo r (changeTempo (recip r) m1)
                                                    -- by Axiom 1
= changeTempo r m0 +:+ changeTempo (r * recip r) m1 -- by Axiom 2
= changeTempo r m0 +:+ changeTempo 1 m1            -- simple arithmetic
= changeTempo r m0 +:+ m1                           -- by Axiom 3
\end{haskellblock}
\end{proof}
For example, this fact justifies the equivalence of the two phrases
shown in \figref{equiv}.

\begin{figure*}
\centerline{
\includegraphics[height=0.6in]{Doc/Pics/equiv}
}
\caption{Equivalent Phrases}
\figlabel{equiv}
\end{figure*}

Many other interesting transformations of Haskore musical objects can
be stated and proved correct using equational reasoning.  We leave as
an exercise for the reader the proof of the following axioms (which
include the above axioms as special cases).

The following axioms are additionally given in a way
which allows automatic tests using the QuickCheck package.
\url{http://www.cs.chalmers.se/~rjmh/QuickCheck/}
The properties are formulated as functions
but they can translated one-by-one
from the axioms stated in mathematical notation.

\begin{haskelllisting}

> module Equivalence where

> import           Haskore.Music hiding (repeat, reverse, dur)
> import qualified Haskore.Music.GeneralMIDI  as MidiMusic
>    -- should also work for general RhyMusic but is a bit more cumbersome
> import qualified Haskore.Performance        as Performance
> import qualified Haskore.Performance.Default as DefltPf
> import qualified Haskore.Performance.Player  as Player

> import qualified Haskore.Basic.Duration as Dur
> import qualified Data.EventList.Relative.TimeTime as TimeListPad
> import qualified Numeric.NonNegative.Wrapper as NonNeg
> import Data.Tuple.HT (mapFst, )

> import Control.Monad.Trans.Reader (runReader, )

> import Test.QuickCheck

\end{haskelllisting}

We define operators \function{=?=} and \function{==?==}
which play the role of our previously defined equivalence sign ``$\equiv$''.
The operator \function{=?=} compares plain pieces of music,
whereas the operator \function{==?==} compares functions mapping to music.
We will use the second one mainly in order to compare
music transformers like \function{changeTempo} and \function{transpose}.

\begin{haskelllisting}

> infix 4 =?=, ==?==

> (=?=) :: MidiMusic.T -> MidiMusic.T -> Bool
> (=?=) m0 m1 =
>     let pl  = DefltPf.map :: Player.Map NonNeg.Rational Rational MidiMusic.Note
>         perform m =
>            mapFst TimeListPad.catMaybes $
>            runReader (Performance.monadFromMusic pl m) DefltPf.context
>     in  perform m0 == perform m1

> (==?==) :: (a -> MidiMusic.T) -> (a -> MidiMusic.T) -> (a -> Bool)
> (==?==) fm0 fm1 x  =  fm0 x =?= fm1 x

\end{haskelllisting}

Here we repeat one of the simple axioms,
now also with a test function ready for quick-checking.

\begin{axiom}
Changing the tempo by $1$ and transposing by $0$ are identities.
That is:
\begin{center}
\code{changeTempo 1} \equivalent \code{id} \\
\code{transpose 0} \equivalent \code{id}
\end{center}

\begin{haskelllisting}

> propTempoNeutral, propTransposeNeutral :: MidiMusic.T -> Bool

> propTempoNeutral = changeTempo 1 ==?== id

> propTransposeNeutral = transpose 0 ==?== id

\end{haskelllisting}

\end{axiom}

The first QuickCheck test function reads as:
``The property of a neutral tempo change is that
changing the tempo by one is equivalent to the identity function.''
It says everything we want to state and not more.
It is available in a machine readable form
ready both for static provers and for tests by execution.
QuickCheck will call these functions on several
randomly generated pieces of music.
These songs might sound awful,
so they should be exotically enough in order to check
whether our axioms are not only true for common music.

\begin{axiom}
\function{changeTempo} is \keyword{multiplicative} and
\function{transpose} is \keyword{additive}.  
That is, for any \code{r0}, \code{r1},
\code{p0}, \code{p1}:
\begin{center}
\code{changeTempo r0 . changeTempo r1} \equivalent \code{changeTempo (r0*r1)}\\
\code{transpose p0 . transpose p1} \equivalent \code{transpose (p0+p1)}
\end{center}
\begin{haskelllisting}

> propTempoTempo ::
>    Dur.Ratio -> Dur.Ratio -> MidiMusic.T -> Property
> propTempoTempo r0 r1 m =
>    r0 > 0 && r1 > 0   ==>
>       (changeTempo r0 . changeTempo r1 ==?==
>        changeTempo (r0*r1)) m

> propTransposeTranspose ::
>    Int -> Int -> MidiMusic.T -> Bool
> propTransposeTranspose p0 p1 =
>    transpose p0 . transpose p1 ==?== transpose (p0+p1)

\end{haskelllisting}

\end{axiom}

The first equation needs the precondition of non-zero tempo changes.
Changing the tempo to zero causes a division by zero
when \function{Pf.fromMusic} recomputes the duration of a whole note.
Because of the precondition we can no longer have \type{Bool} as function value
but we must use \type{Property}
which stores not only the result of the test
but also if the precondition was fulfilled.
Test cases where the precondition fail
do not count in the maximum number of tests performed per test function.

\begin{axiom}
Function composition is \keyword{commutative} with respect to both tempo
scaling and transposition.
That is, for any \code{r0}, \code{r1}, \code{p0} and \code{p1}:
\begin{center}
\code{changeTempo r0 .\ changeTempo r1} \equivalent \code{changeTempo r1 .\ changeTempo r0}\\
\code{transpose p0 .\ transpose p1} \equivalent \code{transpose p1 .\ transpose p0}\\
\code{changeTempo r0 .\ transpose p0} \equivalent \code{transpose p0 .\ changeTempo r0}\\
\end{center}

\begin{haskelllisting}

> propTempoCommutativity :: Dur.Ratio -> Dur.Ratio -> MidiMusic.T -> Property
> propTempoCommutativity r0 r1 m =
>    r0 > 0 && r1 > 0   ==>
>       (changeTempo r0 . changeTempo r1 ==?==
>        changeTempo r1 . changeTempo r0) m

> propTransposeCommutativity :: Int -> Int -> MidiMusic.T -> Bool
> propTransposeCommutativity p0 p1 =
>    transpose p0 . transpose p1 ==?== transpose p1 . transpose p0

> propTempoTransposeCommutativity ::
>    Dur.Ratio -> Int -> MidiMusic.T -> Property
> propTempoTransposeCommutativity r p m =
>    r > 0   ==>
>       (changeTempo r . transpose p ==?==
>        transpose p . changeTempo r) m

\end{haskelllisting}

\end{axiom}

\begin{axiom}
Tempo scaling and transposition are \keyword{distributive} over both
sequential and parallel composition.
That is, for any \code{r}, \code{p}, \code{m0}, and \code{m1}:
\begin{center}
\code{changeTempo r (m0 +:+\ m1)} \equivalent \code{changeTempo r m0 +:+\ changeTempo r m1}\\
\code{changeTempo r (m0 =:=\ m1)} \equivalent \code{changeTempo r m0 =:=\ changeTempo r m1}\\
\code{transpose p (m0 +:+\ m1)} \equivalent \code{transpose p m0 +:+\ transpose p m1}\\
\code{transpose p (m0 =:=\ m1)} \equivalent \code{transpose p m0 =:=\ transpose p m1}
\end{center}

\begin{haskelllisting}

> propTempoSerial, propTempoParallel ::
>    Dur.Ratio -> MidiMusic.T -> MidiMusic.T -> Property

> propTempoSerial r m0 m1 =
>    r > 0   ==>
>       changeTempo r (m0 +:+ m1) =?=
>       changeTempo r m0 +:+ changeTempo r m1

> propTempoParallel r m0 m1 =
>    r > 0   ==>
>       changeTempo r (m0 =:= m1) =?=
>       changeTempo r m0 =:= changeTempo r m1

> propTransposeSerial, propTransposeParallel ::
>    Int -> MidiMusic.T -> MidiMusic.T -> Bool
> propTransposeSerial p m0 m1 =
>    transpose p (m0 +:+ m1) =?= transpose p m0 +:+ transpose p m1
> propTransposeParallel p m0 m1 =
>    transpose p (m0 =:= m1) =?= transpose p m0 =:= transpose p m1

\end{haskelllisting}

\end{axiom}

\begin{comment}
Counter example for propTempoParallel:
r = 1
m0 = c 0 0 []
m1 = d 0 0 [] =:= (d 0 0 [] +:+ c 0 0 [])

This leads to different results
because (=:=) merges parallel compositions in the operands.
This is suppressed if an identity like (changeTempo 1) or (transpose 0) is inserted.
\end{comment}

\begin{axiom}
Sequential and parallel composition are \keyword{associative}.
That is, for any \code{m0}, \code{m1}, and \code{m2}:
\begin{center}
\code{m0 +:+\ (m1 +:+\ m2)} \equivalent \code{(m0 +:+\ m1) +:+\ m2}\\
\code{m0 =:=\ (m1 =:=\ m2)} \equivalent \code{(m0 =:=\ m1) =:=\ m2}
\end{center}

\begin{haskelllisting}

> propSerialAssociativity, propParallelAssociativity ::
>    MidiMusic.T -> MidiMusic.T -> MidiMusic.T -> Bool
> propSerialAssociativity m0 m1 m2 =
>    m0 +:+ (m1 +:+ m2) =?= (m0 +:+ m1) +:+ m2
> propParallelAssociativity m0 m1 m2 =
>    m0 =:= (m1 =:= m2) =?= (m0 =:= m1) =:= m2

\end{haskelllisting}

\end{axiom}

\begin{axiom}
Parallel composition is \keyword{commutative}.
That is, for any \code{m0} and \code{m1}:
\begin{center}
\code{m0 =:=\ m1} \equivalent \code{m1 =:=\ m0}
\end{center}

\begin{haskelllisting}

> propParallelCommutativity ::
>    MidiMusic.T -> MidiMusic.T -> Bool
> propParallelCommutativity m0 m1 =
>    m0 =:= m1 =?= m1 =:= m0

\end{haskelllisting}

\end{axiom}

\begin{comment}
Counter example:
m0 = d 0 0 []
m1 = d 0 0 [] +:+ c 0 0 []

When mergeing using sorting the 'c' must be performed before any 'd'
because all three notes start at the same time.
But in contrast to that we obtain:
Performance.fromMusic (m0 =:= m1) -> [d, d, c]
Performance.fromMusic (m1 =:= m0) -> [d, c, d]
\end{comment}

\begin{axiom}
\code{Rest 0} is a \keyword{unit} for \function{changeTempo} and \function{transpose},
and a \keyword{zero} for sequential and parallel composition.
That is, for any \code{r}, \code{p}, and \code{m}:
\begin{center}
\code{changeTempo r (Rest 0)} \equivalent \code{Rest 0}\\
\code{transpose p (Rest 0)} \equivalent \code{Rest 0}\\
\code{m +:+\ Rest 0} \equivalent \code{m} \equivalent \code{Rest 0 +:+\ m}\\
\code{m =:=\ Rest 0} \equivalent \code{m} \equivalent \code{Rest 0 =:=\ m}
\end{center}

\begin{haskelllisting}

> propTempoRest0 :: Dur.Ratio -> Property
> propTempoRest0 r =
>    r > 0   ==>
>       changeTempo r (rest 0) =?= rest 0
> propTransposeRest0 :: Int -> Bool
> propTransposeRest0 p = transpose p (rest 0) =?= rest 0

> propSerialNeutral0, propSerialParallel0,
>   propSerialNeutral1, propSerialParallel1 ::
>     MidiMusic.T -> Bool
> propSerialNeutral0 m  =  m +:+ rest 0 =?= m
> propSerialNeutral1 m  =  rest 0 +:+ m =?= m
> propSerialParallel0 m  =  m =:= rest 0 =?= m
> propSerialParallel1 m  =  rest 0 =:= m =?= m

\end{haskelllisting}

\end{axiom}

\begin{exercise} Establish the validity of each of the above axioms.
\end{exercise}

