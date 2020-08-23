\section{The Architecture of Haskore}

\figref{haskore} shows the overall structure of Haskore.  Note the
independence of high level structures from the ``music platform'' on
which Haskore runs.  Originally, the goal was for Haskore compositions
to run equally well as conventional midi-files \cite{midi},
NeXT MusicKit score files \cite{musickit}
\footnote{The NeXT music platform is obsolete.},
and CSound score files \cite{csound}
\footnote{There also exists a translation to CSound for an earlier version of Haskore.},
and for Haskore compositions to be displayed and
printed in traditional notation using the CMN (Common Music Notation) subsystem.
\footnote{We have abandoned CMN entirely,
as there are now better candidates for notation packages
into which Haskore could be mapped.}
In reality, three platforms are currently supported:
MIDI, CSound, and some signal processing routines written in Haskell.
For musical notation an interface to Lilypond is currently in progress.


\begin{figure*}
\centerline{
\input{Doc/Pics/haskore.tex}
}
\caption{Overall System Diagram}
\figlabel{haskore}
\end{figure*}

In any case, the independence of abstract musical ideas from the
concrete rendering platform is accomplished by having abstract notions
of \keyword{musical object}, \keyword{player}, \keyword{instrument}, and
\keyword{performance}.  All of this resides in the box labeled ``Haskore'' in
the diagram above.

At the module level, Haskore is organized as follows:
\begin{haskelllisting}

> module Haskore (
>    module Haskore.Music,
>    module Haskore.Performance,
>    module Haskore.Performance.Player,
>    module Haskore.Interface.MIDI.Write,
>    module Haskore.Interface.MIDI.Read,
>    module Haskore.Interface.MIDI.Render,
>    module Sound.MIDI.File.Save,
>    module Sound.MIDI.File.Load,
>    ) where
>
> import qualified Haskore.Music
> import qualified Haskore.Performance
> import qualified Haskore.Performance.Player
> import qualified Haskore.Interface.MIDI.Write
> import qualified Haskore.Interface.MIDI.Read
> import qualified Haskore.Interface.MIDI.Render
> import qualified Sound.MIDI.File.Save
> import qualified Sound.MIDI.File.Load

\end{haskelllisting}

\begin{comment}

> import Prelude hiding (repeat, reverse)

\end{comment}

This document was written in the \keyword{literate programming style}, and
thus the \LaTeX\ manuscript file from which it was generated is an
\keyword{executable Haskell program}.  It can be compiled under \LaTeX\ in
two ways: a basic mode provides all of the functionality that most
users will need, and an extended mode in which various pieces of
lower-level code are provided and documented as well (see file header
for details).  This version was compiled in
\basic{basic}\extended{extended} mode.  The document can be retrieved
via WWW from: \url{http://haskell.org/haskore/} (consult the README
file for details).  It is also delivered with the standard joint
Nottingham/Yale Hugs release.

The Haskore code conforms to Haskell 1.4, and has been tested under
the June, 1998 release of Hugs 1.4.  Unfortunately Hugs does not yet
support mutually recursive modules, so all references to the
\module{Player} in this document are commented out, which in effect
makes it part of \module{Performance} (with which it is mutually
recursive).

A final word before beginning: As various musical ideas are presented
in this Haskore tutorial, I urge the reader to question the design
decisions that are made.  There is no supreme theory of music that
dictates my decisions, and what I present is actually one of several
versions that I have developed over the years (this version is much
richer than the one described in \cite{haskore}; it is the ``Haskore
in practice'' version alluded to in \secref{midi} of that paper).  I
believe that this version is suitable for many practical purposes, but
the reader may wish to modify it to better satisfy her intuitions
and/or application.
