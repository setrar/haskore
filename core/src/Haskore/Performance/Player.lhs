
\begin{haskelllisting}

> module Haskore.Performance.Player where
>
> import Haskore.Music (PhraseAttribute, )
> import qualified Haskore.Music as Music
> -- import qualified Haskore.Performance.Context as Context
>    -- this import would cause a cycle
> import qualified Haskore.Performance as Pf
> -- import qualified Data.EventList.Relative.TimeBody    as TimeList
> import qualified Data.EventList.Relative.TimeTime  as TimeListPad
> import qualified Data.EventList.Relative.TimeMixed as TimeListPad
> import qualified Haskore.Basic.Duration as Dur
> import qualified Numeric.NonNegative.Class as NonNeg
> import Haskore.Performance (eventDur, eventDynamics, )
> import Data.Tuple.HT (mapFst, )

> import Control.Monad.Trans.Reader(Reader, asks, )
> import Control.Monad (liftM, )
>
> type T    time dyn note = Pf.Player time dyn note
> -- constructors can't be renamed, we might use a function instead
> -- cons = Pf.PlayerCons
>
> type Name               = Music.PlayerName
> type Map  time dyn note = Pf.PlayerMap time dyn note
>
>
> type PhraseInterpreter time dyn note =
>    PhraseAttribute -> (Pf.T time dyn note, time) -> (Pf.T time dyn note, time)
>
> type EventModifier time dyn note =
>    Pf.Event time dyn note -> Pf.Event time dyn note
>
> changeVelocity :: Num dyn => (dyn -> dyn) ->
>    EventModifier time dyn note
> changeVelocity f =
>    (\e -> e {eventDynamics = f (eventDynamics e)})
>
> changeDur :: Num time => (time -> time) ->
>    EventModifier time dyn note
> changeDur f =
>    (\e -> e {eventDur = f (eventDur e)})

\end{haskelllisting}


\figref{fancy-Player} defines a relatively sophisticated player
called \function{fancyPlayer} that knows all that \function{Player.deflt} knows, and
much more.

All three articulations \constructor{Staccato}, \constructor{Legato},
\constructor{Slurred} are interpreted
as changing the duration of the notes proportionally.
That's why they have the suffix \code{Rel} for {\em relative}.
\begin{itemize}
\item
The function \function{legatoRel}
takes a ratio of each note's duration.
In order to obtain a real Legato effect
the value must be larger than 1.
\item
The function \function{slurredRel} is similar to \function{legatoRel}
but it doesn't extend the duration of the {\em last} note(s).
\item
The function \function{staccatoRel}
divides the note durations by constant factor.
In order to obtain a real Staccato effect
the value must be larger than 1.
\end{itemize}


\begin{haskelllisting}

> staccatoRel, legatoRel, slurredRel :: (NonNeg.C time, Fractional time) =>
>    Dur.T -> Pf.Monad time dyn note -> Pf.Monad time dyn note
> staccatoRel x = mapEvents     (changeDur (/ Dur.toNumber x))
> legatoRel   x = mapEvents     (changeDur (* Dur.toNumber x))
> slurredRel  x = mapInitEvents (changeDur (* Dur.toNumber x))

> mapInitEvents :: (NonNeg.C time, Num time) =>
>    EventModifier time dyn note ->
>        Pf.Monad time dyn note -> Pf.Monad time dyn note
> mapInitEvents f =
>    let -- modify durations of all notes except those with the latest start time
>        aux =
>           TimeListPad.flatten .
>           TimeListPad.mapTimeInit
>              (TimeListPad.mapBodyInit
>                  (TimeListPad.mapBody (map (fmap f)))) .
>           TimeListPad.collectCoincident
>    in  liftM (mapFst aux)

> mapEvents :: EventModifier time dyn note ->
>                  Pf.Monad time dyn note -> Pf.Monad time dyn note
> mapEvents f = liftM (mapFst (TimeListPad.mapBody (fmap f)))

\end{haskelllisting}

In contrast to the relative interpretations above,
we feel that somehow absolute changes are more useful.
That's why we make these functions the default for the fancy player.
These function expect regular note durations,
that is ratios of a whole note.
\begin{itemize}
\item
The functions \function{legatoAbs} and \function{slurredAbs}
prolong notes by a fix amount.
That is the overlap (if no rests are between) is constant.
\item
\function{staccatoAbs} replaces the note durations by a fix amount.
\end{itemize}

\begin{haskelllisting}

> staccatoAbs, legatoAbs, slurredAbs :: (NonNeg.C time, Fractional time) =>
>    Dur.T -> Pf.Monad time dyn note -> Pf.Monad time dyn note
> staccatoAbs dur pf =
>    getDurModifier const dur >>= flip mapEvents pf
> legatoAbs dur pf =
>    getDurModifier (+)   dur >>= flip mapEvents pf
> slurredAbs dur pf =
>    getDurModifier (+)   dur >>= flip mapInitEvents pf
>
> getDurModifier :: (Fractional time) =>
>    (time -> time -> time) -> Dur.T ->
>       Reader (Pf.Context time dyn note) (EventModifier time dyn note)
> getDurModifier f dur =
>    do tempo <- asks Pf.contextDur
>       return (changeDur (f (Dur.toNumber dur * tempo)))

\end{haskelllisting}

The behavior of \expression{(Ritardando x)} can be explained as
follows.  We'd like to ``stretch'' the time of each event by a factor
from $0$ to $x$, linearly interpolated based on how far along the
musical phrase the event occurs.  I.e., given a start time $t_0$ for
the first event in the phrase, total phrase duration $D$, and event
time $t$, the new event time $t'$ is given by:
\[ t'   = \left(1 + \frac{t-t_0}{D}\cdot x\right)\cdot(t-t_0) + t_0 \]
Further, if $d$ is the duration of the event, then the end of
the event $t+d$ gets stretched to a new time $t_d'$ given by:
\[ t_d' = \left(1 + \frac{t+d-t_0}{D}\cdot x\right)\cdot(t+d-t_0) + t_0 \]
The difference $t_d' - t'$ gives us the new, stretched duration $d'$,
which after simplification is:
\[ d' = \left(1 + \frac{2\cdot(t-t_0)+d}{D}\cdot x\right)\cdot d \]
\constructor{Accelerando} behaves in exactly the same way, except that it
shortens event times rather than lengthening them.  And, a similar but
simpler strategy explains the behaviors of \constructor{Crescendo} and
\constructor{Diminuendo}.


\begin{haskelllisting}

> accent :: (Fractional dyn) =>
>    Rational -> Pf.Monad time dyn note -> Pf.Monad time dyn note
> accent x = mapEvents (changeVelocity (fromRational x +))

\end{haskelllisting}
