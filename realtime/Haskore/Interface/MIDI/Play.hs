{- |
This module used shell-haskell and a pipe
in order to play MIDI music without temporary files.
Today it uses 'System.Process.runInteractiveProcess'.

I got this running only with GHC and Linux so far.
Though it is not satisfying.
It seems that @timidity@ forks itself away
so that it doesn't block the prompt of Haskell.
I assume that because sometimes ghci tries
to write to the pipe before @timidity@ can read it.
Interestingly if I start @timidity@ with an input pipe
from the console it stays synchronously.

Some music seems to be to large,
maybe larger than a pipe buffer,
then the buffer runs over and the replay fails.
E.g. ChildSong6 is short enough, Kantate147 is too long.

Using a pipe is the only way to play
infinite streams of music using an external program.
It is no problem to control a MIDI device
over an unlimited time,
but unfortunately the MIDI file format
uses absolute time points and a file length value.
Thus it is not possible to play infinite MIDI streams
via the MIDI file format!
For better realtime support on Linux look at the @alsa-midi@ package.
-}
module Haskore.Interface.MIDI.Play (play, playSimple, ) where

import Haskore.RealTime.ShellPipe (launch)
import Haskore.RealTime.Utility (catchCtrlC, )

import qualified Haskore.Music.GeneralMIDI   as MidiMusic
import qualified Haskore.Music.Rhythmic      as RhyMusic
import qualified Haskore.Performance.Context as Context

import qualified Haskore.Interface.MIDI.InstrumentMap as InstrMap
import qualified Haskore.Interface.MIDI.Write         as WriteMidi
import qualified Haskore.Interface.MIDI.Render        as Render

import qualified Sound.MIDI.File.Save         as SaveMidi
import qualified Data.ByteString.Lazy as B
import qualified System.IO as IO

import qualified Numeric.NonNegative.Class as NonNeg


play ::
   (Ord instr, Ord drum,
    NonNeg.C time, RealFrac time, Fractional time, RealFrac dyn) =>
   (InstrMap.ChannelProgramPitchTable drum,
    InstrMap.ChannelProgramTable instr,
    Context.T time dyn (RhyMusic.Note drum instr),
    RhyMusic.T drum instr)
      -> IO ()
play = playRaw [] . SaveMidi.toByteString . WriteMidi.fromRhythmicMusicMixed

playSimple :: MidiMusic.T -> IO ()
playSimple = playRaw [] . SaveMidi.toByteString . Render.mixedGeneralMidi

playRaw :: [String] -> B.ByteString -> IO ()
playRaw args stream =
   do
      catchCtrlC

--      (input,_,_) <- launch "play"
--          (["play", "-r","11025","-t","sw","-"])
--      (input,_,_) <- launch "od"
--          (["od", "-t", "x4z"])
      (input,_,_) <- launch "timidity"
          (["timidity", "-B", "8,9"] ++ args ++ ["-"])
      B.hPutStr input stream
      IO.hClose input
