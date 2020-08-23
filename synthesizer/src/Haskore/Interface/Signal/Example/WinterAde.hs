{-# LANGUAGE NoImplicitPrelude #-}
module Haskore.Interface.Signal.Example.WinterAde where

import qualified Synthesizer.Plain.Filter.Recursive.Comb as Comb
import qualified Synthesizer.Plain.Signal as Sig

import qualified Haskore.Interface.Signal.InstrumentMap as InstrMap
import qualified Haskore.Interface.Signal.Write as MusicSignal
import Haskore.Interface.Signal.Write(NonNegTime, Time, Volume)

import           Haskore.Melody.Standard as StdMelody
import qualified Haskore.Music           as Music
import           Haskore.Music.Rhythmic  as RhyMusic
import qualified Haskore.Performance.Fancy as FancyPf
import qualified Haskore.Performance.Context as Context
import qualified Haskore.Basic.Duration  as Dur

import qualified Algebra.Field              as Field
import qualified Algebra.Ring               as Ring

import Synthesizer.Plain.Displacement (mix)
import Synthesizer.Plain.Filter.NonRecursive (fadeInOut)
import Synthesizer.Plain.Instrument (bell, squareBell, fatSaw, filterSweep, )
import qualified Synthesizer.Plain.File as File

import System.Exit(ExitCode)

import NumericPrelude.Base
import NumericPrelude.Numeric

-- import qualified Prelude as P


------------ The song description -------------

melody0, melody1,
 partA, partB, partC0, partC1,
 partD0, partD1, partE0, partE1,
 bassLine0, bassLine1, chordPad
   :: StdMelody.T


-- Melody:
melody0 = Music.transpose 12 (line [partA, partA, partB, partC0, partD0, partE0])
melody1 = Music.transpose 12 (line [partA, partA, partB, partC1, partD1, partE1])

-- Easily create a chord of note of the same length
chordDur :: t -> [t -> NoteAttributes -> StdMelody.T] -> StdMelody.T
chordDur dr chd = chord (map (\n -> n dr na) chd)

partA  = line [chordDur  qn [g  0, c  1, e  1],
               chordDur  qn [g  0, c  1, e  1],
               chordDur  qn [g  0, b  0, d  1],
               chordDur dhn [e  0, g  0, c  1]  ]

partB  = line [chordDur  qn [g  0, c  1, e  1],
               chordDur  qn [g  0, d  1, f  1],
               chordDur  qn [g  0, e  1, g  1],
               chordDur  qn [a  0, d  1, g  1],
               chordDur  en [a  0, d  1, f  1],
               chordDur  en [a  0, d  1, e  1],
               chordDur  qn [a  0, c  1, f  1]  ]

partC0 = line [chordDur  qn [g  0, b  0, d  1],
               chordDur  qn [g  0, c  1, e  1],
               chordDur  qn [g  0, d  1, f  1],
               chordDur  qn [g  0, c  1, f  1],
               chordDur  en [g  0, c  1, e  1],
               chordDur  en [g  0, c  1, d  1],
               chordDur  qn [g  0, b  0, e  1]  ]

partC1 = line [chordDur  qn [g  0, b  0, d  1],
               chordDur  qn [g  0, c  1, e  1],
               chordDur  qn [g  0, d  1, f  1],
               chordDur  qn [gs 0, d  1, f  1],
               chordDur  en [gs 0, c  1, e  1],
               chordDur  en [gs 0, c  1, d  1],
               chordDur  qn [a  0, c  1, e  1]  ]

partD0 = line [chordDur  qn [a  0, c  1, e  1],
               chordDur  qn [a  0, c  1, e  1],
               chordDur  qn [g  0, d  1, f  1],
               chordDur dhn [g  0, c  1, g  1]  ]

partD1 = line [chordDur  qn [g  0, c  1, e  1],
               chordDur  qn [g  0, c  1, e  1],
               chordDur  qn [a  0, c  1, f  1],
               chordDur dhn [c  1, e  1, g  1]  ]

partE0 = line [chordDur  qn [f  0, a  0, e  1],
               chordDur  qn [f  0, a  0, e  1],
               chordDur  qn [g  0, b  0, d  1],
               chordDur dhn [e  0, g  0, c  1]  ]

partE1 = line [chordDur  qn [fs 0, a  0, e  1],
               chordDur  qn [fs 0, a  0, e  1],
               chordDur  qn [g  0, b  0, d  1],
               chordDur dwn [e  0, g  0, c  1]  ]


-- Bass:


{- was intended for simplifying the creation of the bass line
   but sounds awful here

bassPattern = [(0,en),(1,en),(2,en),(1,en),(3,en),(1,en),
               (0,en),(1,en),(4,en),(1,en),(3,en),(1,en)]

bassLine x = transpose (-24) (RhyMusic.fromStdMelody "bass" (line (chordPattern (cycle bassPattern) 0 x)))

bassLine0 = bassLine (bassAAB ++ bassC0 ++ bassDE)
bassLine1 = bassLine (bassAAB ++ bassC1 ++ bassDE)

bassAAB  =    [([(0, C),(0, G),(1, C),(1, D),(1, E)],15*qn),
               ([(0, D),(0, A),(1, D),(1, E),(1, F)], 3*qn) ]

bassC0   =    [([(0, D),(0, G),(1, D),(1, E),(1, F)], 3*qn),
               ([(0, E),(0, G),(1, E),(1, F),(1, G)], 2*qn),
               ([(0, E),(0, B),(1, E),(1, F),(1, G)],   qn) ]

bassC1   =    [([(0, D),(0, G),(1, D),(1, E),(1, F)], 3*qn),
               ([(0, E),(0,Gs),(1, E),(1, F),(1,Gs)], 2*qn),
               ([(0, E),(0, A),(1, E),(1, F),(1, G)],   qn) ]

bassDE   =    [([(0, E),(0, G),(1, C),(1, D),(1, E)], 2*qn),
               ([(0, C),(0, F),(1, C),(1, D),(1, E)],   qn),
               ([(0, C),(0, G),(1, C),(1, D),(1, E)], 3*qn),
               ([(0, C),(0, F),(1, C),(1, D),(1, E)], 2*qn),
               ([(0, D),(0, G),(1, D),(1, E),(1, F)],   qn),
               ([(0, C),(0, G),(1, C),(1, D),(1, E)], 3*qn) ]

chordPattern :: [(Int,Dur)] -> Dur -> [([Pitch], Dur)] -> [StdMelody.T]
chordPattern _ _ [] = []
chordPattern p@((patnote,patdur):pats) chordplayed c@((chordpitchs,chordDur):chords) =
    if   chordplayed >= chordDur
    then chordPattern p (chordplayed-chordDur) chords
    else (Note (chordpitchs!!patnote) patdur []):(chordPattern pats (chordplayed+patdur) c)
-}

bassLine :: [Dur -> NoteAttributes -> StdMelody.T] -> StdMelody.T
bassLine x =
   Music.transpose (-12) (line (map (\n -> n en na) x))

bassLine0 = bassLine (bassA ++ bassA ++ bassB ++ bassC0 ++ bassD0 ++ bassE0)
bassLine1 = bassLine (bassA ++ bassA ++ bassB ++ bassC1 ++ bassD1 ++ bassE1)


bassA, bassB, bassC0, bassD0, bassE0,
 bassC1, bassD1, bassE1 :: [Dur -> NoteAttributes -> StdMelody.T]

bassA  = [c  0, g  0, c  1, g  0, d  1, g  0,
          c  0, g  0, e  1, g  0, d  1, g  0 ]

bassB  = [c  0, g  0, c  1, g  0, e  1, g  0,
          d  0, a  0, d  1, a  0, f  1, a  0 ]

bassC0 = [d  0, g  0, d  1, g  0, f  1, g  0,
          c  0, g  0, c  1, g  0, b  0, g  0 ]

bassD0 = [e  0, a  0, e  1, a  0, g  0, d  1,
          c  0, g  0, c  1, g  0, g  1, c  1 ]

bassE0 = [c  0, f  0, c  1, f  0, d  0, d  1,
          c  0, g  0, c  1, g  0, d  1, g  0 ]

bassC1 = [d  0, g  0, d  1, g  0, f  1, g  0,
          e  0, gs 0, e  1, gs 0, e  1, a  0 ]

bassD1 = [c  0, g  0, c  1, g  0, d  1, g  0,
          c  0, g  0, e  1, g  0, d  1, g  0 ]

bassE1 = [d  0, fs 0, d  1, fs 0, d  1, g  0,
          c  0, g  0, c  1, g  0, d  1, g  0 ]


-- Chord pad:
chordPad = Music.transpose (-12) (
           line [chordDur (15*qn) [g  0, c  1, e  1],
                 chordDur ( 2*qn) [a  0, d  1, f  1],
                 chordDur (   qn) [a  0, c  1, f  1],
                 chordDur ( 3*qn) [b  0, d  1, g  1],
                 chordDur ( 2*qn) [b  0, e  1, gs 1],
                 chordDur (   qn) [c  1, e  1, a  1],
                 chordDur ( 2*qn) [g  0, c  1, e  1],
                 chordDur (   qn) [a  0, c  1, f  1],
                 chordDur ( 3*qn) [c  1, e  1, g  1],
                 chordDur ( 2*qn) [fs 0, a  0, e  1],
                 chordDur (   qn) [g  0, b  0, d  1],
                 chordDur ( 6*qn) [e  0, g  0, c  1] ] )



----------- Configuration of the player -----------

data Instrument =
     Bell
   | Bass
   | Pad
      deriving (Eq, Ord)

type Music = RhyMusic.T () Instrument


context :: Context.T NonNegTime Volume (RhyMusic.Note () Instrument)
context = MusicSignal.contextMetro 120 qn


instrMap :: InstrMap.InstrumentTable Time Volume Instrument
instrMap =
   [(Bell, MusicSignal.amplify (0.2::Volume) bell      ),
    (Bass, MusicSignal.amplify (0.2::Volume) squareBell),
    (Pad,  MusicSignal.amplify (0.2::Volume) fatSaw    )]




--------- Create signals for the parts, apply effects, put them together -------

defltSampleRate :: Ring.C a => a
defltSampleRate = 11025

-- Volume type arises from Haskore
songToSignalMono :: Time -> Music -> Sig.T Volume
songToSignalMono dif song =
   MusicSignal.fromRhythmicMusic defltSampleRate
      (MusicSignal.detuneInstrs dif instrMap)
      FancyPf.map
      context
      song

songToSignalStereo :: Time -> Music -> Sig.T (Volume,Volume)
songToSignalStereo det song =
   zip (songToSignalMono (1-det) song)
       (songToSignalMono (1+det) song)

melodySignal :: StdMelody.T -> Sig.T (Volume,Volume)
melodySignal mel =
   let (musr, musl) = unzip (songToSignalStereo 0.001
                               (RhyMusic.fromStdMelody Bell mel))
   in  zip (Comb.run (round (0.19*defltSampleRate :: Time)) (0.4::Volume) musl)
           (Comb.run (round (0.23*defltSampleRate :: Time)) (0.5::Volume) musr)

melodySignal0, melodySignal1 :: Sig.T (Volume,Volume)
melodySignal0 = melodySignal melody0
melodySignal1 = melodySignal melody1


durToSampleNum :: Music.Dur -> Int
durToSampleNum dr =
   round (defltSampleRate * Context.getDur context * Dur.toNumber dr)


fadeChord :: Field.C a => [a] -> [a]
fadeChord =
   fadeInOut
      (durToSampleNum (2 * dhn))
      (durToSampleNum (Music.dur chordPad - 4 * dhn))
      (durToSampleNum (2 * dhn))

chordSignal :: Sig.T (Volume,Volume)
chordSignal =
   let (musr, musl) = unzip (songToSignalStereo 0.005
                               (RhyMusic.fromStdMelody Pad chordPad))
       filt phase mus = filterSweep defltSampleRate phase (fadeChord mus)
   in  zip (filt (0.7::Volume) musl)
           (filt (0.8::Volume) musr)


bassSignal :: StdMelody.T -> Sig.T (Volume,Volume)
bassSignal mel =
   songToSignalStereo 0.005 (RhyMusic.fromStdMelody Bass mel)

bassSignal0, bassSignal1 :: Sig.T (Volume,Volume)
bassSignal0 = bassSignal bassLine0
bassSignal1 = bassSignal bassLine1


songSignal :: Sig.T (Volume,Volume)
songSignal =
   foldl1 mix [melodySignal0, bassSignal0] ++
   foldl1 mix [melodySignal1, bassSignal1, chordSignal]


main :: IO ExitCode
main =
   File.writeStereoToInt16 "WinterAde.aiff" defltSampleRate songSignal
