\subsubsection{The Score File}
\seclabel{score-file}

\begin{haskelllisting}

> module Haskore.Interface.CSound.Score where
>
> import Haskore.Interface.CSound (Instrument, showInstrumentNumber, PField, Time)
> import qualified Haskore.Interface.CSound.Note as CSNote
> import qualified Haskore.Interface.CSound.Generator as Generator
> import Haskore.Interface.CSound.Generator
>           (compSine1, lineSeg1, randomTable, PStrength, RandDist(Uniform))
>
> import qualified Haskore.Music.Rhythmic        as RhyMusic
> import qualified Haskore.Performance           as Performance
> import qualified Haskore.Performance.BackEnd   as PerformanceBE
> import qualified Haskore.Performance.Context   as Context
> import qualified Haskore.Performance.Fancy     as FancyPf
> import qualified Data.EventList.Relative.TimeBody as TimeList
> import qualified Data.EventList.Absolute.TimeBody as TimeListAbs
> import qualified Haskore.Basic.Pitch           as Pitch
> import qualified Haskore.Interface.CSound.InstrumentMap as InstrMap
> import qualified Haskore.Interface.CSound.SoundMap as SoundMap

> import qualified Numeric.NonNegative.Class as NonNeg

\end{haskelllisting}

We will represent a score file as a sequence of \keyword{score statements}:
\begin{haskelllisting}

> type T = [Statement]

\end{haskelllisting}
The {\tt Statement} data type is designed to simulate CSound's three
kinds of score statements:
\begin{enumerate}
\item A \keyword{tempo} statement, which sets the tempo.  In the absence
of a tempo statement, the tempo defaults to 60 beats per minute.

\item A \keyword{note event}, which defines the start time, pitch,
duration (in beats), volume (in decibels), and instrument to play a
note (and is thus more like a Haskore {\tt Event} than a Midi event,
thus making the conversion to CSound easier than to Midi, as we shall
see later).  Each note event also contains a number of optional
arguments called \keyword{p-fields}, which determine other properties of
the note, and whose interpretation depends on the instrument that
plays the note.  This will be discussed further in a later section.

\item \keyword{Function table} definitions.  A function table is used by
instruments to produce audio signals.  For example, sequencing through
a table containing a perfect sine wave will produce a very pure tone,
while a table containing an elaborate polynomial will produce a
complex sound with many overtones.  The tables can also be used to
produce control signals that modify other signals.  Perhaps the
simplest example of this is a tremolo or vibrato effect, but more
complex sound effects, and FM (frequency modulation) synthesis in
general, is possible.
\end{enumerate}

\begin{haskelllisting}

> data Statement = Tempo Bpm
>                | Note  Instrument StartTime Duration Pch Volume [PField]
>                | Table Table CreatTime TableSize Normalize Generator.T
>      deriving Show
>
> type Bpm       = Int
> type StartTime = Time
> type Duration  = Time
> data Pch       = AbsPch Pitch.Absolute | Cps Float deriving Show
> type Volume    = Float
> type Table     = Int
> type CreatTime = Time
> type TableSize = Int
> type Normalize = Bool

\end{haskelllisting}

This is all rather straightforward, except for function table
generation, which requires further explanation.

\input{Haskore/Interface/CSound/Generator.lhs}

\subparagraph*{Common Tables}

For convenience, here are some common function tables, which take as
argument the identifier integer:
\begin{haskelllisting}

> simpleSine, square, sawtooth, triangle, whiteNoise :: Table -> Statement
>
> simpleSine n = Table n 0 8192 True
>                       (compSine1 [1])
> square     n = Table n 0 1024 True
>                       (lineSeg1 1 [(256, 1), (0, -1), (512, -1), (0, 1), (256, 1)])
> sawtooth   n = Table n 0 1024 True
>                       (lineSeg1 0 [(512, 1), (0, -1), (512, 0)])
> triangle   n = Table n 0 1024 True
>                       (lineSeg1 0 [(256, 1), (512, -1), (256, 0)])
> whiteNoise n = Table n 0 1024 True
>                       (randomTable Uniform)

\end{haskelllisting}
The following function for a composite sine has an extra argument, a
list of harmonic partial strengths:
\begin{haskelllisting}

> compSine :: Table -> [PStrength] -> Statement
> compSine _ s = Table 6 0 8192 True (compSine1 s)

\end{haskelllisting}

\input{Haskore/Interface/CSound/InstrumentMap.lhs}

\paragraph{Converting Haskore Music.T to a CSound Score File}

To convert a {\tt Music.T} value into a CSound score file, we need to:
\begin{enumerate}
\item Convert the {\tt Music.T} value to a {\tt Performance.T}.
\item Convert the {\tt Performance.T} value to a {\tt Score.T}.
\item Write the {\tt Score.T} value to a CSound score file.
\end{enumerate}

We already know how to do the first step.  Steps two and three will be
achieved by the following two functions:
\begin{haskelllisting}

> fromPerformanceBE :: (NonNeg.C time, Num time) =>
>    (time -> Time) ->
>    PerformanceBE.T time CSNote.T -> T

> saveIA :: T -> IO ()

\end{haskelllisting}
The three steps can be put together in whatever way the user wishes,
but the most general way would be this:
\begin{haskelllisting}

> fromRhythmicMusic ::
>    (RealFrac time, NonNeg.C time, RealFrac dyn, Ord drum, Ord instr) =>
>       Tables ->
>       (InstrMap.SoundTable drum,
>        InstrMap.SoundTable instr,
>        Context.T time dyn (RhyMusic.Note drum instr),
>        RhyMusic.T drum instr) -> T
> fromRhythmicMusic tables (dMap, iMap, cont, m) =
>    tables ++ fromRhythmicPerformance dMap iMap
>                 (Performance.fromMusic FancyPf.map cont m)
>
> type Tables = T

\end{haskelllisting}
The \type{Tables} argument is a user-defined set of function tables,
represented as a sequence of {\tt Statement}s (specifically, {\tt
Table} constructors).  (See \secref{function-table}.)

\subparagraph*{From Performance.T to Score.T}

The translation between \type{Performance.Event}s and score
\type{CSoundScore.Note}s is straightforward, the only tricky part being:
\begin{itemize}
\item The unit of time in a {\tt Performance.T} is the second, whereas
in a {\tt Score.T} it is the beat.  However, the default CSound tempo is
60 beats per minute, or one beat per second, as was already mentioned,
and we use this default for our \keyword{score} files.  Thus the two are
equivalent, and no translation is necessary.
\item CSound wants to get pitch information in the form 'a.b'
but it interprets them very different.
Sometimes it is considered as 'octave.pitchclass'
sometimes it is considered as fraction frequency.
We try to cope with it using the two-constructor type Pch.
\item Like for MIDI data we must
distinguish between Velocity and Volume.
Velocity is instrument dependent and
different velocities might result in different flavors of a sound.
As a quick work-around we turn the velocity information into volume.
Cf. {\tt dbamp} in the CSound manual.
\end{itemize}

\begin{haskelllisting}

> fromPerformanceBE timeMap =
>    map (\(time, event) ->
>       noteToStatement timeMap time
>          (PerformanceBE.eventDur  event)
>          (PerformanceBE.eventNote event)) .
>    TimeListAbs.toPairList .
>    TimeList.toAbsoluteEventList NonNeg.zero
>
> fromRhythmicPerformance ::
>    (RealFrac time, NonNeg.C time, RealFrac dyn, Ord drum, Ord instr) =>
>    InstrMap.SoundTable drum ->
>    InstrMap.SoundTable instr ->
>       Performance.T time dyn (RhyMusic.Note drum instr) -> T
> fromRhythmicPerformance dMap iMap =
>    fromPerformanceBE realToFrac .
>    PerformanceBE.fromPerformance
>       (CSNote.fromRhyNote
>          (InstrMap.lookup dMap)
>          (InstrMap.lookup iMap))
>
> fromRhythmicPerformanceMap ::
>    (RealFrac time, NonNeg.C time, RealFrac dyn) =>
>    InstrMap.ToSound drum ->
>    InstrMap.ToSound instr ->
>       Performance.T time dyn (RhyMusic.Note drum instr) -> T
> fromRhythmicPerformanceMap dMap iMap =
>    fromPerformanceBE realToFrac .
>    PerformanceBE.fromPerformance (CSNote.fromRhyNote dMap iMap)
>
> fromRhythmicPerformanceWithAttributes ::
>    (RealFrac time, NonNeg.C time, RealFrac dyn) =>
>    SoundMap.DrumTableWithAttributes out drum ->
>    SoundMap.InstrumentTableWithAttributes out instr ->
>       Performance.T time dyn (RhyMusic.Note drum instr) -> T
> fromRhythmicPerformanceWithAttributes dMap iMap =
>    fromRhythmicPerformanceMap
>       (SoundMap.lookupDrum dMap)
>       (SoundMap.lookupInstrument iMap)
>
> noteToStatement ::
>    (time -> Time) -> time -> time ->
>       CSNote.T -> Statement
> noteToStatement timeMap t d (CSNote.Cons pfs v i p) =
>    Note i (timeMap t) (timeMap d)
>           (maybe (Cps 0 {- dummy -}) AbsPch p) v pfs

\end{haskelllisting}

\subparagraph*{From Score to Score File}

Now that we have a value of type {\tt Score}, we must write it into a
plain text ASCII file with an extension {\tt .sco} in a way that
CSound will recognize.  This is done by the following function:
\begin{haskelllisting}

> saveIA s =
>    do putStr "\nName your score file "
>       putStr "(.sco extension will be added): "
>       name   <- getLine
>       save (name ++ ".sco") s

> save :: FilePath -> T -> IO ()
> save name s = writeFile (name ++ ".sco") (toString s)

\end{haskelllisting}
This function asks the user for the name of the score file, opens that
file for writing, writes the score into the file using the function
\function{toString}, and then closes the file.

The score file is a plain text file containing one statement per line.
Each statement consists of an opcode, which is a single letter that
determines the action to be taken, and a number of arguments.  The
opcodes we will use are ``e'' for end of score, ``t'' to set tempo,
``f'' to create a function table, and ``i'' for note events.
\begin{haskelllisting}

> toString :: T -> String
> toString s = unlines (map statementToString s ++ ["e"])   -- end of score

\end{haskelllisting}

Finally, the \function{statementToString} function:
\begin{haskelllisting}

> statementToString :: Statement -> String
> statementToString = unwords . statementToWords
>
> statementToWords :: Statement -> [String]
> statementToWords (Tempo t) =
>    ["t", "0", show t]
> statementToWords (Note i st d p v pfs) =
>    ["i", showInstrumentNumber i, show st, show d,
>     pchToString p, show v] ++ map show pfs
> statementToWords (Table t ct s n gr)   =
>    ["f", show t, show ct, show s,
>     (if n then id else ('-':))
>        (unwords (Generator.toStatementWords gr))]
>
> -- it's exciting whether CSound knows what we mean with the values
> -- (0 < note) is for compatibility with older CSound example files
> pchToString :: Pch -> String
> pchToString (AbsPch ap) =
>    let (oct, note) = divMod ap 12
>    in  show oct ++ "." ++
>        (if 0 < note && note < 10 then "0" else "") ++
>        show note
> pchToString (Cps freq) = show freq

\end{haskelllisting}
