-- Partial Encoding of Chick Corea's ``Children's Song No. 6''

module Haskore.Interface.Signal.Example.ChildSong6 where

import qualified Haskore.Example.ChildSong6 as ChildSong6
import Synthesizer.Plain.Instrument (fastBell)
import qualified Synthesizer.Plain.Signal as Sig
import qualified Synthesizer.Plain.File as File

import qualified Haskore.Music.GeneralMIDI as Music
import Haskore.Interface.Signal.Write(Time, Volume)

import qualified Haskore.Performance.Fancy as FancyPf
import qualified Haskore.Interface.Signal.Write as MusicSignal

import System.Exit (ExitCode)



-- Volume type arises from Haskore
songToSignalMono :: Time -> Time -> Music.T -> Sig.T Volume
songToSignalMono sampleRate dif song =
   MusicSignal.fromRhythmicMusic
      sampleRate
      (MusicSignal.detuneInstrs dif
          [(Music.AcousticGrandPiano,
            MusicSignal.amplify (0.1::Volume) fastBell)])
      FancyPf.map
      (MusicSignal.contextMetro 60 Music.qn)
      song

songSignal :: Time -> Time -> Sig.T Volume
songSignal sampleRate dif =
   songToSignalMono sampleRate dif (Music.transpose 12 ChildSong6.song)


stereoSignal :: Time -> Sig.T (Volume,Volume)
stereoSignal sampleRate =
   zip (songSignal sampleRate 1.001)
       (songSignal sampleRate 0.999)

main :: IO ExitCode
main =
   File.renderStereoToInt16 "ChildSong6.aiff" 22050 stereoSignal
