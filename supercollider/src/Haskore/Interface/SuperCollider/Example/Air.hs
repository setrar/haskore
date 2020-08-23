{- |
Demonstration song using SuperCollider and global effects.
This was arranged for the HAL2 meeting in July 2007, Leipzig.
-}
module Haskore.Interface.SuperCollider.Example.Air where

-- import qualified Sound.SC3 as SC
import qualified Sound.SC3.UGen.Bindings.DB as SCOsci
import qualified Sound.SC3.UGen.Bindings.DB as SCFilt
import qualified Sound.SC3.UGen.Bindings.DB as SCUGEnv
import qualified Sound.SC3.UGen.Math       as SCMath

import Sound.SC3.UGen.Enum (DoneAction(PauseSynth))
import Sound.SC3.UGen.Rate (Rate(KR))

import qualified Sound.SC3.UGen.Type as SCType
import Sound.SC3.UGen.Type (UGen)

import qualified Haskore.Interface.SuperCollider.Example  as Example
import qualified Haskore.Interface.SuperCollider.Play     as Play
import qualified Haskore.Interface.SuperCollider.Schedule as Schedule
import qualified Haskore.Interface.SuperCollider.SoundMap as SoundMap

import qualified Haskore.Interface.SuperCollider.Schedule.Channel as CSchedule

import Haskore.Interface.SuperCollider.Schedule.Channel
   (rhythmicMusicFromRhythm, rhythmicMusicFromDrum,
    rhythmicMusicFromMelody,
    ugenFromSound, installSound)

import           Haskore.Composition.Chord as Chord

import           Haskore.Melody         as Melody
import qualified Haskore.Music          as Music
import           Haskore.Music.Rhythmic as RhyMusic
import           Haskore.Basic.Duration((%+))

import qualified Haskore.Basic.Duration as Dur

import System.Random (randomRs, mkStdGen)

import Data.List (genericLength)



transposition :: Int
transposition = 2

{-
bassMelody :: Melody.T (Rational, ())
bassMelody =
  Music.line $
  zipWith
     (\vel n -> n (vel%1000, ()))
     (randomRs (0,1000) (mkStdGen 914)) $
-}
bassMelodyA0 :: [() -> Melody.T ()]
bassMelodyA0 =
   a 1 den : c 2 en : a 1 sn : d 2 en : e 2 sn : g 2 en :
   a 1 den : a 1 sn : c 2 en : a 1 sn : c 2 en : d 2 sn : e 2 den :
   a 1 den : c 2 en : a 1 sn : d 2 en : e 2 sn : g 2 en :
   a 1 den : a 1 sn : c 2 en : a 1 sn : c 2 en : a 1 sn : g 1 den :
   []

bassMelodyA1 :: [() -> Melody.T ()]
bassMelodyA1 =
   a 1 den : d 2 en : a 1 sn : e 2 en : fs 2 sn : a 2 en :
   a 1 den : a 1 sn : d 2 en : a 1 sn : d 2 en : e 2 sn : a 2 den :
   a 1 den : d 2 en : a 1 sn : e 2 en : fs 2 sn : a 2 en :
   a 1 den : a 1 sn : c 2 en : d 2 sn : e 2 en : d 2 sn : a 1 den :
   []

bassMelodyA :: [() -> Melody.T ()]
bassMelodyA = bassMelodyA0 ++ bassMelodyA1

-- B == A

bassMelodyC :: [() -> Melody.T ()]
bassMelodyC =
   a 1 den : d 2 en : a 1 sn : e 2 en : f 2 sn : a 2 en :
   a 1 den : a 1 sn : d 2 en : a 1 sn : d 2 en : f 2 sn : a 2 den :
   a 1 den : d 2 en : a 1 sn : e 2 en : f 2 sn : a 2 en :
   a 1 den : a 1 sn : c 2 en : d 2 sn : e 2 en : d 2 sn : a 1 den :
   a 1 den : c 2 en : a 1 sn : d 2 en : e 2 sn : g 2 en :
   a 1 den : a 1 sn : c 2 en : a 1 sn : c 2 en : d 2 sn : e 2 den :
   a 1 den : c 2 en : a 1 sn : d 2 en : e 2 sn : g 2 en :
   a 1 den : a 1 sn : c 2 en : a 1 sn : c 2 en : a 1 sn : g 1 den :
   []

-- D == A

bassMelodyE :: [() -> Melody.T ()]
bassMelodyE =
   a 1 den : d 2 en : a 1 sn : e 2 en : f 2 sn : a 2 en :
   a 1 den : a 1 sn : d 2 en : a 1 sn : d 2 en : f 2 sn : a 2 den :
   a 1 den : d 2 en : a 1 sn : e 2 en : f 2 sn : a 2 en :
   a 1 den : a 1 sn : c 2 en : d 2 sn : e 2 en : d 2 sn : a 1 den :
   a 1 den : c 2 en : a 1 sn : d 2 en : e 2 sn : g 2 en :
   a 1 den : a 1 sn : c 2 en : a 1 sn : c 2 en : d 2 sn : e 2 den :
   a 1 den : c 2 en : a 1 sn : d 2 en : e 2 sn : g 2 en :
   a 1 den : a 1 sn : c 2 en : a 1 sn : c 2 en : a 1 sn : g 1 den :
   []

bassMelodyF :: [() -> Melody.T ()]
bassMelodyF =
   a 1 den : c 2 en : a 1 sn : c 2 en : e 2 sn : f 2 en :
   a 1 den : a 1 sn : c 2 en : a 1 sn : c 2 en : e 2 sn : f 2 den :
   a 1 den : d 2 en : a 1 sn : d 2 en : f 2 sn : a 2 en :
   a 1 den : a 1 sn : d 2 en : e 2 sn : f 2 en : e 2 sn : a 1 den :
   b 1 den : e 2 en : b 1 sn : e 2 en : a 2 sn : b 2 en :
   b 1 den : b 1 sn : e 2 en : b 1 sn : e 2 en : a 2 sn : b 2 den :
   b 1 den : e 2 en : b 1 sn : e 2 en : gs 2 sn : b 2 en :
   b 1 den : b 1 sn : e 2 en : b 1 sn : gs 2 en : e 2 sn : b 1 den :
   []



bassMelody :: Melody.T ()
bassMelody =
  let trans = map (Music.transpose transposition .)
  in  Music.line $
      map
        (Music.crescendo 0.8 . Music.loudness1 0.2 .
         Music.line . map ($())) $
      bassMelodyA : bassMelodyA :
      bassMelodyA : bassMelodyC :
      bassMelodyA : bassMelodyE :
      bassMelodyA : bassMelodyC :
      trans bassMelodyA :
      trans bassMelodyF :
      trans bassMelodyA :
      trans bassMelodyF :
      trans bassMelodyA :
      trans (bassMelodyA0 ++ bassMelodyA0) :
      []

-- cf. Example.Morph
sliceVertInf :: Int -> [a] -> [[a]]
sliceVertInf n =
   map (take n) . iterate (drop n)

harpScale :: Melody.T ()
harpScale =
   -- could use sliceVert or sliceHoriz
   let binomDists = map sum $ sliceVertInf 3 $
          randomRs (-1,1::Double) (mkStdGen 102)
       tones = [d,e,a,b]
       -- properFraction is useless for negative numbers
       splitFraction x =
          let n = floor x
          in  (n, x - fromIntegral n)
       makeNote x =
          let (oct,p) = splitFraction x
          in  (tones!!floor(p*genericLength tones)) oct sn ()
   in  Music.legato qn $ Music.line $ take (round(6*4*dhn/sn)) $
           map makeNote $ zipWith (+) binomDists $
             iterate (0.02+) (-2)


-- envelopedStrings :: Music.Dur -> InstrumentUGen
envelopedStrings :: UGen -> SoundMap.Instrument
envelopedStrings dur params =
   let env = SCUGEnv.envGen KR 1 2 0 1 PauseSynth
                (Example.bridge 3 dur 0.5)
   in  env * Example.strings params


chordConfuse :: Melody.T ()
chordConfuse =
   Chord.harmonicGen () (2*4*dhn) [a 1, b 1, d 2, e  2, g 2]


chordsA :: Melody.T ()
chordsA =
   Chord.harmonicGen () (4*dhn) [a 1, c 2, e  2] +:+
   Chord.harmonicGen () (4*dhn) [a 1, d 2, fs 2] +:+
   Chord.harmonicGen () (4*dhn) [a 2, c 2, e  2] +:+
   (Chord.harmonicGen () (4*dhn) [a 2, d 2] =:=
    (fs 2 (3*dhn) () +:+ g 2 dhn ()))

chordsB :: Melody.T ()
chordsB =
   let ce = Chord.harmonicGen () (4*dhn) [c 2, e 2]
   in  ce +:+
       (d 2 (2*4*dhn) () =:=
          fs 2 (4*dhn) () +:+
          f  2 (4*dhn) ()) +:+
       ce

chordsBlow :: Melody.T ()
chordsBlow =
   a 1 (4*4*dhn) () =:= chordsB

chordsBhigh :: Melody.T ()
chordsBhigh =
   a 2 (4*4*dhn) () =:= chordsB

chordsC :: Melody.T ()
chordsC =
   (a 1 (2*dhn) () +:+ a 2 (2*dhn) ()
      =:= c 2 (4*dhn) () =:= e  2 (4*dhn) ()) +:+
   (d 2 (2*4*dhn) () =:=
    fs 2 (2*dhn) () +:+ g 2 (dhn) () +:+ fs 2 (dhn) () +:+
    f  2 (2*dhn) () +:+ g 2 (dhn) () +:+ f  2 (dhn) () =:=
    a 2 (2*4*dhn) ()) +:+
   (a 2 (4*dhn) () =:= c 2 (4*dhn) () =:= e  2 (4*dhn) ())

chordsD :: Melody.T ()
chordsD =
   (a 1 (2*dhn) () +:+ a 2 (2*dhn) ()
      =:= c 2 (4*dhn) () =:= e  2 (4*dhn) ()) +:+
   (d 2 (4*dhn) () =:=
    fs 2 (2*dhn) () +:+ g 2 (dhn) () +:+ fs 2 (dhn) () =:=
    a 2 (4*dhn) ()) +:+
   (c 2 (2*dhn) () +:+ d 2 (2*dhn) () =:= f 2 (4*dhn) () =:= a 2 (4*dhn) ())

chordsD0 :: Melody.T ()
chordsD0 = chordsD +:+
   (b 1 (4*dhn) () =:=
    e 2 (4*dhn) () =:=
    gs 2 (2*dhn) () +:+ a 2 (dhn) () +:+ b 2 (dhn) ())

chordsD1 :: Melody.T ()
chordsD1 = chordsD +:+
   (b 1 (4*dhn) () =:=
    e 2 (4*dhn) () =:=
    a 2 (2*dhn) () +:+ gs 2 (2*dhn) ())

chordsE :: Melody.T ()
chordsE =
   Chord.harmonicGen () (4*dhn) [a 1, c 2, e  2] +:+
   Chord.harmonicGen () (4*dhn) [a 1, d 2, fs 2] +:+
   Chord.harmonicGen () (8*dhn) [a 1, c 2, e  2]

chords :: Melody.T Double
chords =
   Music.legato wn $ Music.mapDurNote
      (\dur (Melody.Note () p) ->
          Melody.Note (2 * Dur.toNumber dur) p) $
   chordConfuse +:+
   chordsA +:+
   chordsBlow +:+ chordsBhigh +:+
   chordsC +:+
   Music.transpose transposition (chordsD0 +:+ chordsD1 +:+ chordsE)


xylophoneSolo :: Melody.T ()
xylophoneSolo =
   let build n0 notes =
          Music.line $
          map (\n -> n sn () +:+ n0 sn () +:+ n0 sn ()) notes
   in  build (a 1)
          [e 2, d 2, e 2, d 2, e 2, d 2, e 2, d 2,
           e 2, d 2, e 2, g 2, a 2, g 2, e 2, d 2,
           e 2, d 2, e 2, d 2, e 2, d 2, e 2, d 2,
           g 2, a 2, c 3, a 2, d 3, c 3, a 2, g 2,
           f 2, d 2, f 2, d 2, f 2, d 2, f 2, d 2,
           f 2, d 2, f 2, d 2, g 2, d 2, f 2, d 2,
           e 2, d 2, e 2, d 2, e 2, d 2, e 2, d 2,
           a 2, gs 2, g 2, fs 2, f 2, e 2, ds 2, d 2]


bassStringMelody :: Melody.T ()
bassStringMelody =
   Music.transpose transposition $
   Music.line $ map (\n -> n dhn ()) $
      a 1 : b 1 : c 2 : e 2 : fs 2 : g 2 : a 2 : b 2 :
      d 2 : e 2 : f 2 : a 2 : gs 2 : a 2 : b 2 : c 3 :
      []


melodyA :: Melody.T ()
melodyA =
   Music.line $
   e 2 (9%+8) () : d 2 den () : c 2 den () : a 2 dqn () :
   e 2 dqn () : d 2 (5%+16) () : c 2 qn () : d 2 (21%+16) () :
   e 2 den () : a 1 (15%+16) () :
   a 1 (5%+16) () : b 1 qn () : c 2 (wn + 5%+16) () :
   b 1 den () : a 1 den () : d 2 dqn () : e 2 dqn () :
   c 2 dqn () : d 2 den () : a 1 (15%+8 + 21%+16) () :
   []

melodyB :: Melody.T ()
melodyB =
   Music.take (10 + 11%+16) melodyA +:+
   (Music.line $
    g 2 (5%+16) () : fs 2 qn () : f 2 (5%+16) () : e 2 ddqn () :
    [])

melody :: Melody.T ()
melody =
   Music.rest (4*4*dhn)
     +:+ melodyB
     +:+ melodyB
     +:+ Music.rest (4*4*dhn)
     +:+ Music.transpose transposition (melodyA +:+ melodyA)


{-
scsynth -N air.osc _ air.aiff 44100 AIFF int16
-}

schedule :: Schedule.T
schedule =
   CSchedule.fromRhythmicMusic $
   do bass     <- installSound SoundMap.with0Attributes "bass"
                     (Example.fmBass (SCType.mce [0.99,1.01]))
      guitar   <- installSound SoundMap.with0Attributes "guitar"
                     (Example.fmGuitar (SCType.mce [2.99,3.01]))
      strings  <- installSound SoundMap.with1Attribute "strings"
                     envelopedStrings
      harpsichord <- installSound SoundMap.with0Attributes "harpsichord"
                     Example.harpsichord
      bassStrings <- installSound SoundMap.with0Attributes "bassstrings"
                     Example.modulatedStrings
      xylophone <- installSound SoundMap.with0Attributes "xylophone"
                     Example.xylophone
      hihat    <- installSound SoundMap.with0Attributes "hihat"
                     Example.hihat
      bassdrum <- installSound SoundMap.with0Attributes "bassdrum"
                     Example.bassdrum
      let lfoSine = 1000 * exp
             (0.3 * SCOsci.sinOsc KR 0.1 (SCType.mce [-pi/2, 0]) +
              0.3 * SCOsci.sinOsc KR (sqrt 0.03) 0)
          mix = flip SCMath.clip2 1 $ 0.2 * (
            ugenFromSound bass +
            0.3 * ugenFromSound xylophone +
            0.5 * ugenFromSound harpsichord +
            3 * SCFilt.lpf (ugenFromSound bassStrings) 1000 +
            SCFilt.rlpf (0.5 * ugenFromSound strings) lfoSine 0.1 +
            (let guit = 0.6 * ugenFromSound guitar
             in  guit + SCFilt.combN (SCFilt.lpf guit 500) 0.2 0.2 5) +
            ugenFromSound hihat +
            2*ugenFromSound bassdrum)
      return (mix,
         Music.changeTempo 2 $
--         Music.take 10 $
--         Music.drop (4*4*4*dhn) $
         (rhythmicMusicFromMelody strings $ Music.transpose (-12) chords) =:=
         hnr +:+
         (rhythmicMusicFromMelody harpsichord harpScale =:=
          Music.rest (2*4*dhn) +:+ (Music.chord $
            (rhythmicMusicFromMelody bass $ Music.transpose (-36) bassMelody) :
            (rhythmicMusicFromMelody guitar $ Music.transpose (-12) melody) :
            (Music.rest (3*4*4*dhn) +:+
               (rhythmicMusicFromMelody xylophone $ Music.transpose (0) xylophoneSolo)) :
            (Music.rest (5*4*4*dhn) +:+
               (rhythmicMusicFromMelody bassStrings $ Music.transpose (-36) bassStringMelody)) :
            (Music.replicate (7*4*8 + 2) $
                rhythmicMusicFromRhythm bassdrum sn "x.x..x" =:=
                (Music.replicate 6 $ rhythmicMusicFromDrum hihat sn)) :
            [])))


main :: IO ()
main = Play.schedule Play.defaultLatency schedule
