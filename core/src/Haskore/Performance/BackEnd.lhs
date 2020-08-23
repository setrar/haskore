\subsection{Connect Performance to a Back-End}
\seclabel{performance-backend}

\begin{haskelllisting}

> module Haskore.Performance.BackEnd where
>
> import qualified Haskore.Performance    as Pf
> import qualified Haskore.Music          as Music
> import qualified Haskore.Basic.Pitch    as Pitch
> import qualified Data.EventList.Relative.TimeBody    as TimeList
> import qualified Data.EventList.Relative.TimeTime as TimeListPad

> import Haskore.Music ((=:=), (+:+))

\end{haskelllisting}

The performance data structure is still bound to music specific data.
We still have to convert that into back-end specific data,
such as MIDI events, CSound statements, SuperCollider messages or other.
The new data type \type{Performance.BackEnd.T}
is similar to \type{Performance.T},
but does not contain transposition or dynamics information any longer.
Also music-specific data is converted to back-end specific data.

Later we have to provide converters from each type of music
to each back-end.
This requires combinatorial amount of implementation work
but it is the most flexible way to do so.
We expect only a few general types of music which fit to many back-ends,
and many music types specialised to features of a particular back-end.
It would be certainly less work to have an universal intermediate,
but this restricts the flexibility.

\begin{haskelllisting}

> type T      time note = TimeList.T    time (Event time note)
> type Padded time note = TimeListPad.T time (Event time note)
>
> data Event time note =
>        Event {eventDur  :: time,
>               eventNote :: note}
>      deriving (Eq, Ord, Show)

\end{haskelllisting}

Now we provide a function which simplifies conversion
from a \type{Performance.Event} to a \type{Performance.BackEnd.Event}
in case that this conversion does not depend on the event time and duration.

\begin{haskelllisting}

> instance Functor (Event time) where
>    fmap f e = e{eventNote = f (eventNote e)}

> mapTime :: (time0 -> time1) -> T time0 note -> T time1 note
> mapTime f =
>    TimeList.mapBody
>       (\ev -> ev{eventDur = f (eventDur ev)}) .
>    TimeList.mapTime f

> mapTimePadded ::
>    (time0 -> time1) -> Padded time0 note -> Padded time1 note
> mapTimePadded f =
>    TimeListPad.mapBody
>       (\ev -> ev{eventDur = f (eventDur ev)}) .
>    TimeListPad.mapTime f

> eventFromPerformanceEvent ::
>    (dyn -> Pitch.Relative -> note -> backEndNote) ->
>       Pf.Event time dyn note -> Event time backEndNote
> eventFromPerformanceEvent f =
>    \ (Pf.Event dur vel trans note)
>             -> Event dur (f vel trans note)

> fromPerformance ::
>    (dyn -> Pitch.Relative -> note -> backEndNote) ->
>       Pf.T time dyn note -> T time backEndNote
> fromPerformance = TimeList.mapBody . eventFromPerformanceEvent

> fromPaddedPerformance ::
>    (dyn -> Pitch.Relative -> note -> backEndNote) ->
>       Pf.Padded time dyn note -> Padded time backEndNote
> fromPaddedPerformance = TimeListPad.mapBody . eventFromPerformanceEvent

\end{haskelllisting}
For symmetry we also provide a function which converts
a performance back to a music.
This operation is not uniquely defined,
and a satisfying implementation is a music theoretical challenge.
A sophisticated algorithm would have to make assumptions
about the structure of ``common'' music.
So you will be able to construct examples of music
that fool such an algorithm.

The opposite extreme is a version which simply maps
the stream of notes to a big parallel composition
where each parallel channel consists of one note.
(The normal form as described in Hudak's Temporal Media paper.)

The following implementation tries to avoid
obviously unnecessary parallelism
by watching for non-overlapping notes.
Nevertheless the conversion of general polyphonic music
yields a music that is not very nicely structured.
So, don't rely on the structure of the restored music,
only assume that this functions reverts the performance generation.
\begin{haskelllisting}

> toMusic :: T Music.Dur note -> Music.T note
> toMusic =
>    TimeList.switchL
>       (Music.rest 0)
>       (\ (t0, Event d mn) es0 ->
>          let n = if d>=0
>                    then Music.atom d (Just mn)
>                    else error "Performance.toMusic: note of negative duration"
>              rmd =
>                 TimeList.switchL n
>                    (\(t1, re1) es1 ->
>                       if t1 >= d
>                         then n +:+ toMusic (TimeList.cons (t1-d) re1 es1)
>                         else n =:= toMusic es0)
>                    es0
>          in  case compare t0 0 of
>                EQ -> rmd
>                GT -> Music.rest t0 +:+ rmd
>                LT -> error "Performance.toMusic: events in wrong order")

\end{haskelllisting}
