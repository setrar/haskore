\subsection{Convenient Functions for Getting Started With Haskore and MIDI}
\seclabel{test-functions}

{\small
\begin{haskelllisting}

> module Haskore.Interface.MIDI.Render where

> import qualified Haskore.Interface.MIDI.Write         as WriteMidi
> import qualified Haskore.Interface.MIDI.InstrumentMap as InstrMap
> import qualified Sound.MIDI.General          as GeneralMidi

> import qualified Sound.MIDI.File.Save        as SaveMidi
> import qualified Sound.MIDI.File             as MidiFile
> import qualified Sound.MIDI.Message.Channel  as ChannelMsg

> import qualified Haskore.Music.GeneralMIDI   as MidiMusic
> import qualified Haskore.Music.Rhythmic      as RhyMusic
> import qualified Haskore.Music               as Music
> import qualified Haskore.Melody              as Melody
> import qualified Haskore.Performance.Context as Context
> import qualified Haskore.Performance.Fancy   as FancyPerformance

> import qualified Numeric.NonNegative.Class   as NonNeg
> import qualified Numeric.NonNegative.Wrapper as NonNegW

> import System.Process (rawSystem, )
> import System.Exit (ExitCode, )

\end{haskelllisting}
}

Given a \code{Player.Map}, \code{Context.T}, \code{InstrMap.T},
and file name, we can write a \code{MidiMusic.T} value into a midi file:

{\small
\begin{haskelllisting}

> fileFromRhythmicMusic ::
>    (Ord instr, Ord drum, NonNeg.C time, RealFrac time, RealFrac dyn) =>
>    FilePath ->
>       (InstrMap.ChannelProgramPitchTable drum,
>        InstrMap.ChannelProgramTable instr,
>        Context.T time dyn (RhyMusic.Note drum instr),
>        RhyMusic.T drum instr) -> IO ()
> fileFromRhythmicMusic fn m =
>    SaveMidi.toFile fn (WriteMidi.fromRhythmicMusic m)

\end{haskelllisting} }

\subsubsection{Test routines}

Using the defaults above, from a \code{MidiMusic.T} object, we can:

\begin{enumerate}

\item generate a \code{Performance.T}
using \code{Haskore.Performance.Default.fancyFromMusic}

\item generate a \code{MidiFile.T} data structure

{\small
\begin{haskelllisting}

> midi :: MidiMusic.T -> MidiFile.T
> midi =
>    WriteMidi.fromRhythmicPerformance [] InstrMap.defltGM .
>    FancyPerformance.floatFromMusic

> generalMidi :: MidiMusic.T -> MidiFile.T
> generalMidi =
>    WriteMidi.fromGMPerformanceAuto .
>    FancyPerformance.floatFromMusic

> generalMidiDeflt :: MidiMusic.T -> MidiFile.T
> generalMidiDeflt =
>    WriteMidi.fromGMPerformance (InstrMap.lookup InstrMap.defltCMap) .
>    FancyPerformance.floatFromMusic

> mixedMidi :: MidiMusic.T -> MidiFile.T
> mixedMidi =
>    WriteMidi.fromRhythmicPerformanceMixed [] InstrMap.defltGM .
>    FancyPerformance.floatFromMusic

> mixedGeneralMidi :: MidiMusic.T -> MidiFile.T
> mixedGeneralMidi =
>    WriteMidi.fromGMPerformanceMixedAuto .
>    FancyPerformance.floatFromMusic

\end{haskelllisting} }

\item generate a MIDI file

{\small
\begin{haskelllisting}

> fileFromGeneralMIDIMusic :: FilePath -> MidiMusic.T -> IO ()
> fileFromGeneralMIDIMusic filename = SaveMidi.toFile filename . generalMidi

\end{haskelllisting} }

\item generate and play a MIDI file on Windows 95, Windows NT, or Linux

{\small
\begin{haskelllisting}

> fileName :: FilePath
> fileName = "test.mid"

> play :: String -> [String] -> MidiMusic.T -> IO ExitCode
> play cmd opts m =
>    do fileFromGeneralMIDIMusic fileName m
>       rawSystem cmd (opts ++ [fileName])
>
> playWin95, playWinNT,
>    playLinux, playAlsa, playTimidity, playTimidityJack :: MidiMusic.T -> IO ExitCode
> playWin95        = play "mplayer" []
> playWinNT        = play "mplay32" []
> playLinux        = play "playmidi" ["-rf"]
> playAlsa         = play "pmidi" ["-p 128:0"]
> playTimidity     = play "timidity" ["-B8,9"]
> playTimidityJack = play "timidity" ["-Oj"]

\end{haskelllisting} }

\end{enumerate}

Alternatively, just run \code{fileFromGeneralMIDIMusic "test.mid" m} manually,
and then invoke the midi player
on your system using \code{playTest}, defined below for NT:

{\small
\begin{haskelllisting}

> playTest :: IO ExitCode
> playTest =
>    rawSystem "mplay32" [fileName]

\end{haskelllisting} }

\subsubsection{Some General Midi test functions}

Use these functions with caution.

A General Midi user patch map; i.e. one that maps GM instrument names
to themselves, using a channel that is the patch number modulo 16.
This is for use ONLY in the code that follows, o/w channel duplication
is possible, which will screw things up in general.

{\small
\begin{haskelllisting}

> gmUpm :: InstrMap.ChannelProgramTable MidiMusic.Instr
> gmUpm =
>    zipWith
>       (\instr chan ->
>          (instr, (chan, GeneralMidi.instrumentToProgram instr)))
>       GeneralMidi.instruments
>       (cycle $ map ChannelMsg.toChannel [0..15])

\end{haskelllisting} }

Something to play each "instrument group" of 8 GM instruments;
this function will play a C major arpeggio on each instrument.

{\small
\begin{haskelllisting}

> gmTest :: Int -> IO ()
> gmTest i =
>    let gMM = take 8 (drop (i*8) GeneralMidi.instruments)
>        mu  = Music.line (map simple gMM)
>        simple instr = MidiMusic.fromMelodyNullAttr instr Melody.cMajArp
>    in  fileFromRhythmicMusic fileName
>           ([], gmUpm, FancyPerformance.context ::
>                    Context.T NonNegW.Float Float MidiMusic.Note, mu)

\end{haskelllisting} }
