\subsection{CSound}
\seclabel{csound}

\newcommand\genparagraph[1]{
\hypertarget{csound-gen{#1}}{\subparagraph*{GEN{#1}.}}
}
\newcommand\refgen[1]{\hyperlink{csound-gen{#1}}{GEN{#1}}}

\begin{haskelllisting}

> module Haskore.Interface.CSound where

\end{haskelllisting}

[Note: if this module is loaded into Hugs98, the following error
message may result:
\begin{haskelllisting}
    Reading file "CSound.lhs":
    ERROR "CSound.lhs" (line 707):
    *** Cannot derive Eq OrcExp after 40 iterations.
    *** This may indicate that the problem is undecidable.  However,
    *** you may still try to increase the cutoff limit using the -c
    *** option and then try again.  (The current setting is -c40)
\end{haskelllisting}
This is apparently due to the size of the {\tt OrcExp} data type.  For
correct operation, start Hugs with a larger cutoff limit, such as {\tt
-c1000}.]

CSound is a software synthesizer that allows its user to create a
virtually unlimited number of sounds and instruments.  It is extremely
portable because it is written entirely in C.  Its strength lies
mainly in the fact that all computations are performed in software, so
it is not reliant on sophisticated musical hardware.  The output of a
CSound computation is a file representing the signal which can be
played by an independent application, so there is no hard upper limit
on computation time.  This is important because many sophisticated
signals take much longer to compute than to play.  The purpose of this
module is to create an interface between Haskore and CSound in order
to give the Haskore user access to all the powerful features of a
software sound synthesizer.

CSound takes as input two plain text files: a \keyword{score} (.sco) file
and an \keyword{orchestra} (.orc) file.  The score file is similar to a
Midi file, and the orchestra file defines one or more
\keyword{instrument}s that are referenced from the score file (the orchestra
file can thus be thought of as the software equivalent of Midi
hardware).  The CSound program takes these two files as input, and
produces a \keyword{sound file} as output, usually in {\tt .wav} format.
Sound files are generally much larger than Midi files, since they
describe the actual sound to be generated, represented as a sequence
of values (typically 44,100 of them for each second of music), which
are converted directly into voltages that drive the audio speakers.
Sound files can be played by any standard media player found on
conventional PC's.

Each of these files is described in detail in the following sections.

Here are some common definitions:
\begin{haskelllisting}

> newtype Instrument = Instrument Int
>    deriving (Show, Eq)

> instrument :: Int -> Instrument
> instrument = Instrument

> instruments :: [Instrument]
> instruments = map instrument [1..]

> instrumentToNumber :: Instrument -> Int
> instrumentToNumber (Instrument n) = n

> showInstrumentNumber :: Instrument -> String
> showInstrumentNumber = show . instrumentToNumber

> type Name = String

> type Velocity  = Float
> type PField    = Float
> type Time      = Float

\end{haskelllisting}

\input{Haskore/Interface/CSound/Score.lhs}
\input{Haskore/Interface/CSound/Orchestra.lhs}
