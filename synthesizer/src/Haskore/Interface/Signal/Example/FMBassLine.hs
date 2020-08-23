{-# LANGUAGE NoImplicitPrelude #-}
module Haskore.Interface.Signal.Example.FMBassLine where

import qualified Synthesizer.Plain.Filter.NonRecursive as FiltNR
import qualified Synthesizer.Plain.Oscillator as Osci
import qualified Synthesizer.Plain.Signal as Sig
import           Synthesizer.Plain.Instrument (fmBell)
import qualified Synthesizer.Plain.File as File

import qualified Haskore.Basic.Pitch as Pitch
import qualified Haskore.Melody          as Melody
import           Haskore.Melody.Standard as StdMelody
import qualified Haskore.Music           as Music
import           Haskore.Music.Standard  as StdMusic
import qualified Haskore.Performance.Fancy as FancyPf
import qualified Haskore.Interface.Signal.Note  as Note
import qualified Haskore.Interface.Signal.Write as MusicSignal
import Haskore.Interface.Signal.Write(Time, Volume)

import qualified Number.NonNegative as NonNeg

import System.Random (randomRs, mkStdGen, )
import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module         as Module
import qualified Algebra.RealRing       as RealRing

import System.Exit(ExitCode)

import NumericPrelude.Base
import NumericPrelude.Numeric

------------ The song description -------------


data MyNote =
      Bass ModIndex ModDepth Pitch.T
    | Saw  Volume Vibrato Pitch.T
   deriving (Show, Eq, Ord)

type ModIndex = Int
type ModDepth = Time
type ModParam = (ModIndex, ModDepth)

type Vibrato  = Time

type NoteIon = Music.Dur -> () -> Melody.T ()


makeBassLine :: [NoteIon] -> [ModIndex] -> [ModDepth] -> Music.T MyNote
makeBassLine mel indexes depths = line
   (zipWith3 (\n index depth ->
                 Music.mapNote
                    (\(Melody.Note _ p) -> (Bass index depth p))
                    (n en ()))
             mel indexes depths)

bassPattern :: [NoteIon]
bassPattern =
   cycle [a 0, a 0, a 0, e 1, a 0, a 0, f 1, a 0, a 0, d 1, a 0, a 0]

bassLine :: Music.T MyNote
bassLine =
   transpose (-12)
      (makeBassLine bassPattern
         (randomRs (1,4)   (mkStdGen 12354))
         (randomRs (0,2)   (mkStdGen 35902)))


makeSawLine :: [([NoteIon], [(Dur, Dur, Vibrato)])] -> Music.T MyNote
makeSawLine = line . concatMap
   (\(chrd, params) ->
        map (\(durTone,durRest,vib) ->
               let mkNote dur vel =
                      chord (map (\n ->
                                     Music.mapNote
                                        (\(Melody.Note _ p) ->
                                          (Saw vel vib p))
                                        (n dur ()))
                                 chrd)
               in  mkNote durTone 1 +:+ mkNote durRest 0.3)
            params)

zn :: Dur
zn = NonNeg.fromNumber zero

sawPattern :: [([NoteIon], [(Dur, Dur, Vibrato)])]
sawPattern =
   let v0 = (sn,sn,0.01)
       v1 = (en,zn,0.02)
       v2 = (qn,zn,0.05)
       v3 = (qn,zn,0.00)
   in  cycle [([a 0, c 1, e 1], replicate 8 v0 ++ [v0,v0,v0,v1]),
              ([g 0, b 0, d 1], [v2]),
              ([a 0, c 1, e 1], [v3]),
              ([a 0, d 1, f 1], replicate 8 v0 ++ [v0,v0,v0,v1]),
              ([a 0, e 1, g 1], [v2]),
              ([a 0, d 1, f 1], [v3])]

sawLine :: Music.T MyNote
sawLine = makeSawLine sawPattern

song :: Music.T MyNote
song = bassLine =:= sawLine


----------- Configuration of the player -----------


noteToSignal ::
   Time -> Volume -> Pitch.Relative -> MyNote -> Note.T Volume Volume
noteToSignal detune dyn trans note =
   let freq p = detune * Note.pitchFromStd trans p
   in  Note.Cons (\sampleRate ->
          case note of
             Bass index depth p ->
                FiltNR.amplifyVector (dyn * 0.3)
                    (fmBell sampleRate depth (fromIntegral index) (freq p))
             Saw vel vib p ->
                FiltNR.amplifyVector (dyn * vel*0.4)
                    (saw sampleRate vib 10 (freq p)))

saw :: (Module.C a a, Trans.C a, RealRing.C a) =>
             a -> a -> a -> a -> Sig.T a
saw sampleRate modDepth modFreq freq =
   Osci.freqModSaw 0
      (map (\y -> freq/sampleRate * (1+modDepth*y))
           (Osci.staticSine 0.25 (modFreq/sampleRate)))

-- Volume type arises from Haskore
songToSignalMono :: Time -> Time -> Music.T MyNote -> Sig.T Volume
songToSignalMono sampleRate dif music =
   MusicSignal.fromMusic
      sampleRate (noteToSignal dif)
      FancyPf.map
      (MusicSignal.contextMetro 240 qn)
      music

stereoSignal :: Time -> Sig.T (Volume,Volume)
stereoSignal sampleRate =
   zip (songToSignalMono sampleRate 1.003 song)
       (songToSignalMono sampleRate 0.997 song)

main :: IO ExitCode
main =
   File.renderStereoToInt16 "FMBassLine.aiff" 44100
      (\sampleRate -> take (round (16*sampleRate)) (stereoSignal sampleRate))
