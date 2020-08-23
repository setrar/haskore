\subsubsection{Percussion}

Percussion is a difficult notion to represent in the abstract, since
in a way, a percussion instrument is just another instrument, so why
should it be treated differently?  On the other hand, even common
practice notation treats it specially, even though it has much in
common with non-percussive notation.  The midi standard is equally
ambiguous about the treatment of percussion: on one hand, percussion
sounds are chosen by specifying an octave and pitch, just like any
other instrument, on the other hand these notes have no tonal meaning
whatsoever: they are just a convenient way to select from a large
number of percussion sounds.  Indeed, part of the General Midi
Standard is a set of names for commonly used percussion sounds.

\begin{haskelllisting}

> module Haskore.Composition.Drum
>    (T, GM.Drum(..), Element(..), na,
>     toMusic, toMusicDefaultAttr,
>     lineToMusic, elementToMusic, funkGroove) where

> import Haskore.Composition.Trill
> import qualified Haskore.Basic.Duration as Duration
> import Haskore.Basic.Duration (qn, en, )
> import Haskore.Music (qnr, enr, (=:=), changeTempo, rest, )
> import Haskore.Melody.Standard (NoteAttributes, na, )

> import qualified Haskore.Music             as Music
> import qualified Haskore.Music.GeneralMIDI as MidiMusic
> import qualified Haskore.Music.Rhythmic    as RhyMusic
> import qualified Sound.MIDI.General as GM

> type T = GM.Drum

\end{haskelllisting}

Since Midi is such a popular platform, we can at least define some
handy functions for using the General Midi Standard.  We start by
defining the datatype shown in \figref{percussion}, which
borrows its constructor names from the General Midi standard.  The
comments reflecting the ``Midi Key'' numbers will be explained later,
but basically a Midi Key is the equivalent of an absolute pitch in
Haskore terminology.
We will not adapt the MIDI treatment of drums in Haskore
since it makes no sense,
e.g. to transpose drums by increasing the key number.
Thus we defined a special constructor for drums in \type{RhyMusic.T}.
We will now give a function which constructs a \type{RhyMusic.T}
for a given value specifying a drum:
\begin{haskelllisting}

> toMusic :: drum -> Duration.T -> NoteAttributes -> RhyMusic.T drum instr
> toMusic drm dr nas =
>    Music.atom dr (Just (RhyMusic.noteFromAttrs nas (RhyMusic.Drum drm)))

> toMusicDefaultAttr ::
>    drum -> Duration.T -> RhyMusic.T drum instr
> toMusicDefaultAttr drm dr = toMusic drm dr na

\end{haskelllisting}

For example, here are eight bars of a simple rock or ``funk groove''
that uses \function{Drum.toMusic} and \function{Drum.roll}:
\begin{haskelllisting}

> funkGroove :: MidiMusic.T
> funkGroove =
>     let p1 = toMusic GM.LowTom        qn na
>         p2 = toMusic GM.AcousticSnare en na
>     in changeTempo 3 (Music.take 8 (Music.repeat
>          ( (Music.line [p1, qnr, p2, qnr, p2,
>                         p1, p1, qnr, p2, enr])
>            =:= roll en (toMusic GM.ClosedHiHat 2 na) )
>        ))

\end{haskelllisting}

We can go one step further by defining
our own little ``percussion datatype'':
\begin{haskelllisting}

> data Element =
>      R                Duration.T                -- rest
>    | N                Duration.T NoteAttributes -- note
>    | Roll  Duration.T Duration.T NoteAttributes -- roll w/duration
>    | Rolln Integer    Duration.T NoteAttributes -- roll w/number of strokes
>
> lineToMusic :: T -> [Element] -> MidiMusic.T
> lineToMusic dsnd =
>    Music.line . map (elementToMusic dsnd)

> elementToMusic :: T -> Element -> MidiMusic.T
> elementToMusic dsnd el =
>    let drum = toMusic dsnd
>    in  case el of
>           R            dur     -> rest dur
>           N            dur nas -> drum dur nas
>           Roll  sDur   dur nas -> roll sDur (drum dur nas)
>           Rolln nTimes dur nas -> rollN nTimes (drum dur nas)

\end{haskelllisting}
