{-# LANGUAGE NoImplicitPrelude #-}
module Haskore.Interface.Signal.Example.FilterSaw where

import qualified Synthesizer.Plain.Control    as Ctrl
import qualified Synthesizer.Plain.Oscillator as Osci
import qualified Synthesizer.Plain.Filter.Recursive.Comb as Comb
import qualified Synthesizer.Plain.Filter.Recursive.Allpass as Allpass
import qualified Synthesizer.Plain.Signal as Sig
import Synthesizer.Plain.Instrument (filterSaw, )
import qualified Synthesizer.Plain.File as File

import qualified Haskore.Basic.Pitch as Pitch
import qualified Haskore.Melody          as Melody
import           Haskore.Melody.Standard as StdMelody
import qualified Haskore.Music           as Music
import           Haskore.Music.Standard  as StdMusic
import qualified Haskore.Performance.Fancy as FancyPf
import qualified Haskore.Interface.Signal.Note  as Note
import qualified Haskore.Interface.Signal.Write as MusicSignal
import Haskore.Interface.Signal.Write(Time,Volume)

import System.Random (randomRs, mkStdGen, )
-- import Algebra.Additive as Additive(zero)
import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module         as Module

import System.Exit(ExitCode)
import qualified Data.List as List

import NumericPrelude.Base hiding (take, )
import NumericPrelude.Numeric

------------ The song description -------------


data MyNote = FilterSaw Double Pitch.T
   deriving (Show, Eq, Ord)

type Resonance = Time


pattern :: Music.T MyNote
pattern = makePattern patternList (Osci.staticSine 0 0.05)

makePattern :: [Music.Dur -> Resonance -> Melody.T Resonance] ->
   [Resonance] -> Music.T MyNote
makePattern mel reson = line
   (zipWith (\n r -> Music.mapNote
                        (\(Melody.Note r' p) -> (FilterSaw r' p))
                        (n qn r))
            mel reson)

patternList :: [Music.Dur -> Resonance -> Melody.T Resonance]
patternList = concatMap (List.take 16 . cycle) (
   [[a 0, b 0, c 1, e 1],
    [a 0, b 0, c 1, f 1],
    [a 0, d 1, e 1, f 1],
    [a 0, b 0, c 1, e 1]])


----------- Configuration of the player -----------


noteToSignal ::
   Time -> Volume -> Pitch.Relative -> MyNote -> Note.T Volume Volume
noteToSignal detune _ trans (FilterSaw reso p) =
   Note.Cons (\sampleRate ->
      filterSaw sampleRate ((2000::Time)  *  2 ** (1.5*reso))
         (detune * Note.pitchFromStd trans p))

-- Volume type arises from Haskore
songToSignalMono :: Time -> Volume -> Music.T MyNote -> Sig.T Volume
songToSignalMono sampleRate dif song =
   MusicSignal.fromMusic
      sampleRate (noteToSignal dif)
      FancyPf.map
      (MusicSignal.contextMetro 480 qn)
      song

songSignal :: Time -> Sig.T Volume
songSignal sampleRate = songToSignalMono sampleRate 1 pattern

{- use allpass with different parameters for both stereo channels
   in order to 'spread' the frequency spectrum over the stereo panorama -}
allpassChannel ::
   (Module.C a v, Trans.C a) => a -> a -> [v] -> [v]
allpassChannel sampleRate sign x =
   let order = 10
   in  Allpass.cascade order
          (map ((\(Allpass.Parameter k) -> Allpass.Parameter (sign*k)) .
                (Allpass.flangerParameter order))
               (Ctrl.exponential2 (3*sampleRate) (2000/sampleRate))
               --(repeat (50/sampleRate))
           ) x

stereoSignal :: Time -> Sig.T (Volume,Volume)
stereoSignal sampleRate =
   zip (allpassChannel sampleRate ( 1 :: Volume) (songSignal sampleRate))
       (allpassChannel sampleRate (-1 :: Volume) (songSignal sampleRate))

reverb :: Time -> Sig.T (Volume,Volume) -> Sig.T (Volume,Volume)
reverb sampleRate =
   Comb.runMulti
      (List.take 8 (randomRs (round(0.10*sampleRate::Time),
                              round(0.50*sampleRate::Time))
                             (mkStdGen 12354)))
      (0.1::Volume)

reverbedSignal :: Time -> Sig.T (Volume,Volume)
reverbedSignal sampleRate =
   reverb sampleRate
      (stereoSignal sampleRate ++
       List.replicate (round (2*sampleRate)) zero)

main :: IO ExitCode
main =
   File.renderStereoToInt16 "FilterSaw.aiff" 22050 reverbedSignal
