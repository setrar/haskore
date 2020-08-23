{-# LANGUAGE NoImplicitPrelude #-}
module Haskore.Interface.Signal.Example.SwanLake where

import qualified Synthesizer.Plain.Filter.NonRecursive as FiltNR
import qualified Synthesizer.Plain.Filter.Recursive.Moog as Moog
import qualified Synthesizer.Plain.Signal as Sig
import Synthesizer.Plain.Filter.Recursive (Pole(Pole))
import Synthesizer.Plain.Control(exponential2)
import Synthesizer.Plain.Instrument (simpleSaw)
import qualified Synthesizer.Plain.File as File

import qualified Number.Ratio as Ratio
import qualified Algebra.Transcendental      as Trans
import qualified Algebra.Ring                as Ring

-- import qualified Haskore.Music as Music
import           Haskore.Music.Standard  as StdMusic
import           Haskore.Melody.Standard as StdMelody
import qualified Haskore.Performance.Fancy as FancyPf
import qualified Haskore.Interface.Signal.Write as MusicSignal
import Haskore.Interface.Signal.Write(Time,Volume)

import Data.Accessor.Basic as Accessor

import System.Exit(ExitCode)

import NumericPrelude.Base
import NumericPrelude.Numeric


------------ The song description -------------


-- Comfortable creation of chords of notes of the same length
chordDur :: t -> [t -> NoteAttributes -> StdMelody.T] -> StdMelody.T
chordDur dr chd =
   chord $
      zipWith
         (\n v -> n dr (Accessor.set StdMelody.velocity1 (Ratio.toRational98 v) na))
         chd [1,0.3,0.3,0.3]

chords :: StdMelody.T
chords = transpose 12 (line
               [chordDur  wn [a (-3), e  0, a  0, c  1],
                chordDur  wn [d (-2), f  0, a  0, d  1],
                chordDur  wn [a (-2), e  0, a  0, c  1],
                chordDur  wn [gs(-2), e  0, a  0, c  1],
                chordDur  wn [g (-2), e  0, a  0, c  1],
                chordDur  hn [d (-2), f  0, a  0, d  1],
                chordDur  hn [f (-3), f  0, a  0, c  1],
                chordDur  wn [a (-3), e  0, a  0, c  1]  ])

makeMelody :: [NoteAttributes -> StdMelody.T] -> StdMelody.T
makeMelody mel = transpose 24 (line (map ($ na) mel))

melody :: StdMelody.T
melody  = makeMelody [e 1  wn,
                      a 0  qn, b 0 qn, c 1 qn, d 1 qn,
                      e 1 dhn, c 1 qn,
                      e 1 dhn, c 1 qn,
                      e 1 dhn, a 0 qn,
                      c 1  qn, a 0 qn, f 0 qn, c 1 qn,
                      a 0  wn]

----------- Configuration of the player -----------


defltSampleRate :: Ring.C a => a
defltSampleRate = 44100

-- Volume type arises from Haskore
songToSignalMono :: Volume -> StdMelody.T -> Sig.T Volume
songToSignalMono dif song =
    MusicSignal.fromRhythmicMusic
       defltSampleRate
       (MusicSignal.detuneInstrs dif
           [("string", MusicSignal.amplify (0.5::Volume) simpleSaw)])
       FancyPf.map
       (MusicSignal.contextMetro 120 qn)
       (StdMusic.fromStdMelody "string" song)

songSignal :: Volume -> Sig.T Volume
songSignal dif = songToSignalMono dif chords




polarisator :: Trans.C a => a -> a -> [Pole a]
polarisator sampleRate freq =
   map (flip Pole (freq/sampleRate))
       (exponential2 (0.5*sampleRate) 100)

filterSignal :: Time -> [Pole Volume]
filterSignal dif =
   -- we must remove gaps because a pole frequency 0 let most filters crash
   -- hope that the signal doesn't become too short ...
   filter (/= Pole 0 0)
       (MusicSignal.fromRhythmicMusic
           defltSampleRate
           (MusicSignal.detuneInstrs dif [("filter", polarisator)])
           FancyPf.map
           (MusicSignal.contextMetro 120 qn)
           (StdMusic.fromStdMelody "filter" melody))


filteredSignal :: Volume -> Sig.T Volume
filteredSignal dif = FiltNR.amplifyVector (0.1::Volume)
    (Moog.lowpass 10
        (map (Moog.parameter 10) (filterSignal 1))
        (songSignal (1/dif)))
{-
filteredSignal dif =
   map (\(_,_,x) -> 0.1*x)
       (uniFilter (map uniFilterParam filterSignal)
                   songSignal)
-}

stereoSignal :: Sig.T (Volume,Volume)
stereoSignal = zip (filteredSignal 1.002)
                   (filteredSignal 0.998)

main :: IO ExitCode
main =
   File.writeStereoToInt16 "SwanLake.aiff" defltSampleRate stereoSignal
