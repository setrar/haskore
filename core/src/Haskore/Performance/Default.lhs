\subsection{Conversion functions with default settings}
\seclabel{default-performance}

\subsubsection{Examples of Player Construction}

A ``default player'' called \function{Default.player} (not to be confused with
``deaf player''!) is defined for use when none other is specified in
the score; it also functions as a base from which other players can be
derived.  \function{Default.player} responds only to the \constructor{Velocity} note
attribute and to the \constructor{Accent}, \constructor{Staccato}, and \constructor{Legato}
phrase attributes.  It is defined in \figref{default-Player}.
Before reading this code, recall how players are invoked by the
\function{Performance.fromMusic} function defined in the last section; in particular, note the
calls to \function{playNote} and \function{interpretPhase} defined above.  Then
note:
\begin{enumerate}
\item \function{defltPlayNote} is the only function (even in the definition
of \function{Performance.fromMusic}) that actually generates an event.  It also modifies
that event based on an interpretation of each note attribute by the
function \function{defltNasHandler}.

\item  \function{defltNasHandler} only recognizes the \constructor{Velocity} attribute,
which it uses to set the event velocity accordingly.

\item \function{defltInterpPhrase} calls (mutually recursively)
\function{Performance.fromMusic} to interpret a phrase,
and then modifies the result based on
an interpretation of each phrase attribute by the function
\function{defltInterpPhrase}.

\item \function{defltInterpPhrase} only recognizes the \constructor{Accent},
\constructor{Staccato}, and \constructor{Legato} phrase attributes.
For each of these it uses the numeric argument as a ``scaling'' factor
of the volume (for
\constructor{Accent}) and duration (for \constructor{Staccato} and \constructor{Legato}).
Thus \expression{(Phrase (Legato 1.1) m)} effectively increases the duration
of each note in \expression{m} by 10\% (without changing the tempo).
\end{enumerate}

It should be clear that much of the code in Figure
\ref{default-Player} can be re-used in defining a new player.
For example, to define a player \function{weird} that interprets note
attributes just like \function{Default.player} but behaves differently with
respect to phrase attributes, we could write:
\begin{haskelllisting}
  weird :: T
  weird  = Performance.PlayerCons {
              pname           = "Weirdo",
              playNote        = defltPlayNote defltNasHandler,
              interpretPhrase = liftM . myPhraseInterpreter
              notatePlayer    = defltNotatePlayer ()
           }
\end{haskelllisting}
and then supply a suitable definition of \function{myPhraseInterpreter}.  That
definition could also re-use code, in the following sense: suppose we
wish to add an interpretation for \constructor{Crescendo}, but otherwise
have \function{myPhraseInterpreter} behave just like \function{defltInterpPhrase}.
\begin{haskelllisting}
  myPhraseInterpreter :: PhraseAttribute -> Performance.T time dyn note -> Performance.T time dyn note
  myPhraseInterpreter (Dyn (Crescendo x)) pf = ...
  myPhraseInterpreter  pa                 pf = defltInterpPhrase pa pf
\end{haskelllisting}

\begin{exercise}
Fill in the \expression{...} in the definition of \function{myPhraseInterpreter} according
to the following strategy:  Assume $0<\expression{x}<1$.  Gradually scale
the volume of each event by a factor of $1.0$ through $1.0+\expression{x}$,
using linear interpolation.
\end{exercise}

\begin{exercise}
Choose some of the other phrase attributes and provide interpretations
of them, such as \constructor{Diminuendo}, \constructor{Slurred}, \constructor{Trill}, etc.
(The \function{trill} functions from \secref{basic-examples} may be
useful here.)
\end{exercise}

{\small
\begin{haskelllisting}

> module Haskore.Performance.Default where

> import qualified Haskore.Music       as Music
> import qualified Haskore.Performance as Performance
> import qualified Haskore.Performance.Context as Context
> import qualified Haskore.Performance.Player  as Player

> import qualified Data.EventList.Relative.TimeBody    as TimeList

> import qualified Haskore.Basic.Tempo    as Tempo
> import qualified Haskore.Basic.Duration as Dur

> import qualified Numeric.NonNegative.Class   as NonNeg
> import qualified Numeric.NonNegative.Wrapper as NonNegW

> import Prelude hiding (map)

\end{haskelllisting}
}

\begin{figure}
{\small
\begin{haskelllisting}

> -- default is a reserved keyword
> player ::
>    (NonNeg.C time, Fractional time, Real time, Fractional dyn) =>
>    Player.T time dyn note
> player = map "Default"
>
> -- a default PMap that makes everything into a Default.player
> map ::
>    (NonNeg.C time, Fractional time, Real time, Fractional dyn) =>
>    Player.Name -> Player.T time dyn note
> map pname =
>    Performance.PlayerCons {
>       Performance.name            = pname,
>       Performance.playNote        = playNote,
>       Performance.interpretPhrase = interpretPhrase,
>       Performance.notatePlayer    = notatePlayer ()
>    }
>
> playNote :: (Fractional time, Real time) =>
>    Performance.NoteFun time dyn note
> playNote
>    (Performance.Context curDur _ curKey curVelocity) d note =
>        TimeList.singleton 0
>             (Performance.Event {
>                Performance.eventDur       = Dur.toNumber d * curDur,
>                Performance.eventTranspose = curKey,
>                Performance.eventDynamics  = curVelocity,
>                Performance.eventNote      = note } )
>
> interpretPhrase ::
>    (NonNeg.C time, Fractional time, Fractional dyn) =>
>    Performance.PhraseFun time dyn note
> interpretPhrase (Music.Dyn (Music.Accent   x)) = Player.accent x
> interpretPhrase (Music.Art (Music.Staccato x)) = Player.staccatoAbs x
> interpretPhrase (Music.Art (Music.Legato   x)) = Player.legatoAbs x
> interpretPhrase _                              = id
>
> notatePlayer :: () -> Performance.NotateFun
> notatePlayer _ = ()

> context ::
>    (NonNeg.C time, Fractional time, Real time, Fractional dyn) =>
>    Context.T time dyn note
> context =
>    Performance.Context {
>       Performance.contextPlayer     = player,
>       Performance.contextDur        = Tempo.metro 60 Dur.qn,
>       Performance.contextTranspose  = 0,
>       Performance.contextDynamics   = 1
>    }

\end{haskelllisting}
}
\caption{Definition of default Player \function{Default.player}.}
\figlabel{default-Player}
\end{figure}

{\small
\begin{haskelllisting}

> fromMusic ::
>    (Ord note, NonNeg.C time, RealFrac time, Fractional dyn, Ord dyn) =>
>    Music.T note -> Performance.T time dyn note
> fromMusic =
>    Performance.fromMusic map context
>
> fromMusicModifyContext ::
>    (Ord note, NonNeg.C time, RealFrac time, Fractional dyn, Ord dyn) =>
>    (Context.T time dyn note -> Context.T time dyn note) ->
>    Music.T note ->
>    Performance.T time dyn note
> fromMusicModifyContext update =
>    Performance.fromMusic
>       map
>       (update context)
>
> floatFromMusic :: (Ord note) =>
>    Music.T note -> Performance.T NonNegW.Float Float note
> floatFromMusic = fromMusic
>
> paddedFromMusic  ::
>    (Ord note, NonNeg.C time, RealFrac time, Fractional dyn, Ord dyn) =>
>    Music.T note -> Performance.Padded time dyn note
> paddedFromMusic =
>    Performance.paddedFromMusic map context
>
> paddedFromMusicModifyContext ::
>    (Ord note, NonNeg.C time, RealFrac time, Fractional dyn, Ord dyn) =>
>    (Context.T time dyn note -> Context.T time dyn note) ->
>    Music.T note ->
>    Performance.T time dyn note
> paddedFromMusicModifyContext update =
>    Performance.fromMusic
>       map
>       (update context)

\end{haskelllisting}
}
