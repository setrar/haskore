\subsection{Interpretation and Performance}
\seclabel{performance}

% import Player

\begin{haskelllisting}

> module Haskore.Performance where
>
> import Haskore.Music(PlayerName, PhraseAttribute)

> import qualified Haskore.Basic.Duration as Dur
> import qualified Haskore.Basic.Pitch    as Pitch
> import qualified Haskore.Music          as Music
> import qualified Data.EventList.Relative.TimeBody    as TimeList
> import qualified Data.EventList.Relative.TimeTime    as TimeListPad
> import qualified Data.EventList.Relative.TimeMixed   as TimeListPad
> import qualified Numeric.NonNegative.Class as NonNeg

> import Haskore.General.Utility (maximum0, )
> import Data.Tuple.HT (mapPair, )
> import qualified Data.Record.HT as Record
> import Data.Ord.HT (comparing, )
> import Control.Monad.Trans.Reader (Reader, runReader, ask, asks, local, )
> import Control.Applicative(WrappedMonad(WrapMonad), unwrapMonad, )
> import Data.Traversable(sequenceA)
> import Data.List (foldl')

> import Prelude hiding (Monad)

\end{haskelllisting}

Now that we have defined the structure of musical objects, let us turn
to the issue of \keyword{performance}, which we define as a temporally
ordered sequence of musical \keyword{events}:
\begin{haskelllisting}

> type T      time dyn note = TimeList.T     time (Event time dyn note)
> type Padded time dyn note = TimeListPad.T  time (Event time dyn note)

\end{haskelllisting}

The \type{Padded} performance has a trailing time value.
It can be considered as the duration after the last event
after which the performance finishes.
This need not to be the duration of the last event,
as in the case, where the last note is a short one,
that is played while an earlier long note remains playing.
Another exception is a performance which ends with a rest.

\begin{haskelllisting}

> data Event time dyn note =
>        Event {eventDur       :: time,
>               eventDynamics  :: dyn,
>               eventTranspose :: Pitch.Relative,
>               eventNote      :: note}
>      deriving (Eq, Show)
>
> -- this order is just for the old test cases which rely on it
> instance (Ord time, Ord dyn, Ord note) =>
>              Ord (Event time dyn note) where
>    compare =
>       Record.compare
>          [comparing eventNote,
>           comparing eventDynamics,
>           comparing eventTranspose,
>           comparing eventDur]

\end{haskelllisting}
An event is the lowest of our music representations not yet committed
to Midi, CSound, or the MusicKit.
An event \code{Event \{eventDur = d, eventNote = n\}}
captures the fact that
the note \code{n} respecting all its attributes is played
for a duration \code{d}
(where now duration is measured in seconds, rather than beats).

We introduce the type variables \type{time} and \type{dyn} here
which are used for time and dynamics quantities.
For every-day use where only efficiency counts
you will infer these type variables with \type{Float} or \type{Double}.
For testing the validity of axioms (see \secref{equivalence})
we need exact computation which can be achieved with \type{Rational}.

To generate a complete performance of, i.e.\ give an interpretation
to, a musical object, we must know the time to begin the performance,
and the proper volume, key and tempo.
We must also know what \keyword{player}s to use;
that is, we need a mapping from the \code{PlayerName}s in
an abstract musical object to the actual players to be used.  (We
don't yet need a mapping from abstract \code{Instr}s to instruments,
since this is handled in the translation from a performance into, say,
Midi, such as defined in \secref{midi}.)

We can thus model a performer as a function \code{fromMusic} which maps
all of this information and a musical object into a performance:
\begin{haskelllisting}

> fromMusic ::
>    (NonNeg.C time, RealFrac time, Ord dyn, Fractional dyn, Ord note) =>
>    PlayerMap time dyn note -> Context time dyn note -> Music.T note -> T time dyn note
>
> type PlayerMap time dyn note = PlayerName -> Player time dyn note
> data Context time dyn note =
>    Context {contextDur       :: time,
>             contextPlayer    :: Player time dyn note,
>             contextTranspose :: Pitch.Relative,
>             contextDynamics  :: dyn}
>       deriving Show

> type UpdateContext time dyn note a =
>    (a -> a) -> Context time dyn note -> Context time dyn note
>
> updatePlayer    :: UpdateContext time dyn note (Player time dyn note)
> updatePlayer    f c = c {contextPlayer    = f (contextPlayer c)}
> updateDur       :: UpdateContext time dyn note time
> updateDur       f c = c {contextDur       = f (contextDur c)}
> updateTranspose :: UpdateContext time dyn note Pitch.Relative
> updateTranspose f c = c {contextTranspose = f (contextTranspose c)}
> updateDynamics  :: UpdateContext time dyn note dyn
> updateDynamics  f c = c {contextDynamics  = f (contextDynamics c)}

  fromMusic pmap c@Context {contextStart = t, contextPlayer = pl, contextDur = dt, contextTranspose = k} m =
   case m of
      Note p d nas    -> playNote pl c p d nas
      Rest d          -> []
      m1 :+: m2       -> fromMusic pmap c m1 ++
                         fromMusic pmap (c {contextStart = t + dur m1 * dt}) m2
      m1 :=: m2       -> merge (fromMusic pmap c m1) (fromMusic pmap c m2)
      Tempo  a   m    -> fromMusic pmap (c {contextDur = dt / fromRational a}) m
      Transpose  p  m -> fromMusic pmap (c {contextTranspose = k + p}) m
      Instrument nm m -> fromMusic pmap (c {cInst = nm}) m
      Player nm  m    -> fromMusic pmap (c {contextPlayer = pmap nm}) m
      Phrase pas m    -> interpretPhrase pl pmap c pas m

\end{haskelllisting}

\begin{figure}
\begin{haskelllisting}

> fromMusic pmap c = fst . TimeListPad.viewTimeR . paddedFromMusic pmap c
>
> paddedFromMusic ::
>    (NonNeg.C time, RealFrac time, Ord dyn, Fractional dyn, Ord note) =>
>    PlayerMap time dyn note -> Context time dyn note ->
>       Music.T note -> Padded time dyn note
> paddedFromMusic pmap c =
>    TimeListPad.catMaybes . fst . flip runReader c . monadFromMusic pmap
>
> type PaddedWithRests time dyn note =
>         TimeListPad.T time (Maybe (Event time dyn note))
>
> type Monad time dyn note =
>    Reader
>       (Context time dyn note)
>       (PaddedWithRests time dyn note, time)

> sequenceReader :: [Reader r a] -> Reader r [a]
> sequenceReader = unwrapMonad . sequenceA . map WrapMonad

> combine ::
>    ([performance] -> performance, [time] -> time) ->
>    [Reader r (performance, time)] ->
>    Reader r (performance, time)
> combine f =
>    fmap (mapPair f . unzip) . sequenceReader

> monadFromMusic ::
>    (NonNeg.C time, RealFrac time, Ord dyn, Fractional dyn, Ord note) =>
>    PlayerMap time dyn note -> Music.T note -> Monad time dyn note
>
> monadFromMusic pmap =
>    Music.foldList
>       (\d at -> flip fmap ask $ \c ->
>          let noteDur = Dur.toNumber d * contextDur c
>              events =
>                 maybe
>                    (TimeList.singleton 0 Nothing)
>                    (TimeList.mapBody Just .
>                     playNote (contextPlayer c) c d) at
>          in  (TimeListPad.snocTime events noteDur, noteDur))
>       (\ctrl ->
>          case ctrl of
>             Music.Tempo     a  -> local (updateDur (/ Dur.toNumber a))
>             Music.Transpose p  -> local (updateTranspose (+ p))
>             Music.Player    nm -> local (updatePlayer (const (pmap nm)))
>             Music.Phrase    pa -> \m ->
>                asks contextPlayer >>= \pl -> interpretPhrase pl pa m)
>       (combine (TimeListPad.concat, sum))
>       (combine (foldl' TimeListPad.merge (TimeListPad.pause 0), maximum0))

  This implementation fails on
      mel = a 0 wn () +:+ b 0 wn ()  =:=  rest qn +:+ mel

> {- this does only work if the performance in the Monad does not have a Maybe for each note

> monadFromMusicOld :: (Ord time, Fractional time, Ord note) =>
>    PlayerMap time dyn note -> Music.T note ->
>    Reader (Context time dyn note) (Padded time dyn note, time)
>
> monadFromMusicOld pmap =
>    Music.foldList
>       (\d at -> flip fmap ask $ \c ->
>          let noteDur = fromRational d * contextDur c
>          in  ((case at of
>                  Just note -> playNote (contextPlayer c) c d note
>                  Nothing   -> [],
>                noteDur), noteDur))
>       (\ctrl ->
>          case ctrl of
>             Music.Tempo     a  -> local (updateDur (/ fromRational a))
>             Music.Transpose p  -> local (updateTranspose (+ p))
>             Music.Player    nm -> local (updatePlayer (const (pmap nm)))
>             Music.Phrase    pa -> \m ->
>                asks contextPlayer >>= \pl -> interpretPhrase pl pa m )
>       (combine (TimeListPad.concat, sum))
>       (combine (foldl' TimeListPad.merge ([], 0), maximum0))
> -}

\end{haskelllisting}
\caption{The ``real'' \code{fromMusic} function.}
\figlabel{real-fromMusic}
\end{figure}

Some things to note:
\begin{enumerate}
\item
The function \function{monadFromMusic} does not simply convert
a music object to a performance
but it converts a music to an action (\type{Reader} monad).
Given a context we can start the action by \function{runReader}
and we get an event.
The way \function{monadFromMusic} works
is to build a big action from many small actions.

\item
The \code{Context} is the running ``state'' of the performance, and
gets updated in several different ways.  For example, the
interpretation of the \code{Tempo} constructor involves scaling
the duration of a whole note appropriately and
updating the \code{contextDur} field of the context.

It's better not to manipulate the members of \code{Context} directly,
but to use the abstractions from \code{PerformanceContext}.
This way we can stay independent of the concrete definition of \code{Context}.
(I would like to define this data structure in \code{PerformanceContext}
but the current Haskell compilers
have a complicated handling of mutually dependent modules.)

\item
Interpretation of notes and phrases is player dependent.  Ultimately a
single note is played by the \code{playNote} function, which takes the
player as an argument.  Similarly, phrase interpretation is also
player dependent, reflected in the use of \code{interpretPhrase}.
Precisely how these two functions work is described in \secref{players}.

\item
The \code{Dur} component of the context is the duration,
in seconds, of one whole note.
See \secref{tempo} for assisting functions.

\item
In the treatment of \code{Serial}, note that the sub-sequences are
appended together, with the start time of the second argument delayed
by the duration of the first.  The function \code{dur} (defined in
\secref{basic-examples}) is used to compute this duration.  Note
that this results in a quadratic time complexity for \code{fromMusic}.  A
more efficient solution is to have \code{fromMusic} compute the duration
directly, returning it as part of its result.  This version of \code{fromMusic}
is shown in \figref{real-fromMusic}.

\item
In contrast, the sub-sequences derived from the arguments to \code{Parallel}
are merged into a time-ordered stream.
This is done with \function{merge} from the module \module{Data.EventList.Relative.TimeTime}.
\end{enumerate}


% equivalence of musical values
\input{Test/Equivalence.lhs}


% this section should be moved to the Player module
% as soon as the Haskell interpreters support mutually recursive modules

\subsection{Players}
\seclabel{players}

In the last section we saw how a performance involved the notion of a
 \keyword{player}.  The reason for this is the same as for real players and
their instruments: many of the note and phrase attributes
(see \secref{phrasing}) are player and instrument dependent.
For example, how should ``legato'' be interpreted in a performance?
Or ``diminuendo''?
Different players interpret things in different ways, of course, but
even more fundamental is the fact that a pianist, for example,
realizes legato in a way fundamentally different from the way a
violinist does, because of differences in their instruments.
Similarly, diminuendo on a piano and a harpsichord are different
concepts.

With a slight stretch of the imagination, we can even consider a
``notator'' of a score as a kind of player: exactly how the music is
rendered on the written page may be a personal, stylized process.  For
example, how many, and which staves should be used to notate a
particular instrument?

In any case, to handle these issues, Haskore has a notion of a
\keyword{player} which ``knows'' about differences with respect to performance
and notation.  A Haskore player is a 4-tuple consisting of a name and
three functions: one for interpreting notes, one for phrases, and one
for producing a properly notated score.
\begin{haskelllisting}

> data Player time dyn note =
>         PlayerCons { name            :: PlayerName,
>                      playNote        :: NoteFun time dyn note,
>                      interpretPhrase :: PhraseFun time dyn note,
>                      notatePlayer    :: NotateFun }
>
> instance (Show time, Show dyn) => Show (Player time dyn note) where
>    show p = "Player.cons " ++ name p

> type NoteFun time dyn note =
>      Context time dyn note -> Music.Dur -> note -> T time dyn note
> type PhraseFun time dyn note =
>      PhraseAttribute -> Monad time dyn note -> Monad time dyn note
> type NotateFun = ()

\end{haskelllisting}
The last line above is because notation is currently not implemented.
Note that both \code{NotateFun} and \code{PhraseFun}
functions return a \code{Performance.T}.
