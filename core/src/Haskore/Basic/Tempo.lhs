\subsubsection{Tempo}
\seclabel{tempo}

\begin{haskelllisting}

> module Haskore.Basic.Tempo where

> import qualified Haskore.Basic.Pitch as Pitch
> import Haskore.Basic.Duration (qn, en, sn, (%+), )
> import qualified Haskore.Music as Music
> import Haskore.Music(changeTempo, line, (+:+), (=:=), )
> import qualified Haskore.Melody as Melody

> import qualified Haskore.Basic.Duration as Dur

> import qualified Data.List as List

\end{haskelllisting}

\paragraph*{Set tempo.}

To make it easier to initialize the duration element
of a \code{PerformanceContext.T} (see \secref{performance}),
we can define a ``metronome'' function that,
given a standard metronome marking (in beats per minute)
and the note type associated with one beat (quarter note, eighth note, etc.)
generates the duration of one whole note:
\begin{haskelllisting}

> metro :: Fractional a => a -> Music.Dur -> a
> metro setting dur = 60 / (setting * Dur.toNumber dur)

\end{haskelllisting}

Additionally we define some common tempos and
some range of interpretation as in \figref{tempos}.
This means, the tempo Andante may vary between
\code{fst andanteRange} and \code{snd andanteRange}
beats per minute.
For example, \code{metro andante qn} creates a tempo of 92 quarter
notes per minute.

\begin{figure}
%larghissimoRange = ( 30, 40) -- as slow as reasonably possible
%adagiettoRange   = ( 70, 80) -- slightly faster than adagio
%allegrettoRange  = (110,150) -- not quite allegro
\begin{haskelllisting}

> largoRange, larghettoRange, adagioRange, andanteRange,
>   moderatoRange, allegroRange, prestoRange, prestissimoRange
>     :: Fractional a => (a,a)
>
> largoRange       = ( 40, 60) -- slowly and broadly
> larghettoRange   = ( 60, 68) -- a little less slow than largo
> adagioRange      = ( 66, 76) -- slowly
> andanteRange     = ( 76,108) -- at a walking pace
> moderatoRange    = (108,120) -- at a moderate tempo
> allegroRange     = (120,168) -- quickly
> prestoRange      = (168,200) -- fast
> prestissimoRange = (200,208) -- very fast
>
>
> largo, larghetto, adagio, andante, moderato, allegro,
>   presto, prestissimo :: Fractional a => a
>
> average :: Fractional a => a -> a -> a
> average x y = (x+y)/2
>
> largo       = uncurry average largoRange
> larghetto   = uncurry average larghettoRange
> adagio      = uncurry average adagioRange
> andante     = uncurry average andanteRange
> moderato    = uncurry average moderatoRange
> allegro     = uncurry average allegroRange
> presto      = uncurry average prestoRange
> prestissimo = uncurry average prestissimoRange

\end{haskelllisting}
\caption{Common names for tempo.}
\figlabel{tempos}
\end{figure}

% http://en.wikipedia.org/wiki/Tempo
% http://groups.google.de/groups?q=adagio+andante+allegro+bpm&hl=de&lr=&ie=UTF-8&selm=25919-385E77EA-28%40storefull-615.iap.bryant.webtv.net&rnum=5


\begin{figure*}
\centerline{
\includegraphics[height=2.0in]{Doc/Pics/poly}
}
\caption{Nested Polyrhythms}
\figlabel{polyrhythms}
\end{figure*}

\paragraph*{Polyrhythms.}

For some rhythmical ideas, consider first a simple \keyword{triplet} of
eighth notes; it can be expressed as ``\code{Tempo (3\%2) m}'', where
\code{m} is a line of three eighth notes.  In fact \code{Tempo} can be
used to create quite complex rhythmical patterns.  For example,
consider the ``nested polyrhythms'' shown in \figref{polyrhythms}.
They can be expressed quite naturally in Haskore as follows (note the
use of the \code{where} clause in \code{pr2} to capture recurring
phrases):
\begin{haskelllisting}

> pr1, pr2 :: Pitch.T -> Melody.T ()
> pr1 p =
>    changeTempo (5%+6)
>       (changeTempo (4%+3)
>          (line [mkLn 1 p qn,
>                 changeTempo (3%+2)
>                    (line [mkLn 3 p en,
>                           mkLn 2 p sn,
>                           mkLn 1 p qn] ),
>                  mkLn 1 p qn]) +:+
>          changeTempo (3%+2) (mkLn 6 p en))
>
> pr2 p =
>    changeTempo (7%+6)
>       (line [m1,
>              changeTempo (5%+4) (mkLn 5 p en),
>              m1,
>              mkLn 2 p en])
>    where m1 = changeTempo (5%+4) (changeTempo (3%+2) m2 +:+ m2)
>          m2 = mkLn 3 p en
>
> mkLn :: Int -> Pitch.T -> Music.Dur -> Melody.T ()
> mkLn n p d = line (take n (List.repeat (Melody.note p d ())))

\end{haskelllisting}
To play polyrhythms \code{pr1} and \code{pr2} in parallel using middle C
and middle G, respectively, we would do the following (middle C is in
the 5th octave):
\begin{haskelllisting}

> pr12 :: Melody.T ()
> pr12 = pr1 (5, Pitch.C) =:= pr2 (5, Pitch.G)

\end{haskelllisting}

\paragraph*{Symbolic Meter Changes}

We can implement a notion of ``symbolic meter changes'' of the form
``oldnote = newnote'' (quarter note = dotted eighth, for example) by
defining a function:
\begin{haskelllisting}

> (=/=) :: Music.Dur -> Music.Dur -> Music.T note -> Music.T note
> old =/= new  =  changeTempo (new/old)

\end{haskelllisting}
Of course, using the new function is not much longer than using
\code{changeTempo} directly, but it may have nemonic value.

