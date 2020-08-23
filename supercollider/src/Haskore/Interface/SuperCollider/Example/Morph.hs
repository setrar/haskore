{- |
This was composed for the Teachers '95 morph
of the Georg Cantor school in Halle
in occasion of the 20th birthday in 2008.
-}
module Haskore.Interface.SuperCollider.Example.Morph where

-- import qualified Sound.SC3 as SC
import qualified Sound.SC3.UGen.Bindings.DB as SCOsci
import qualified Sound.SC3.UGen.Bindings.DB as SCFilt
-- import qualified Sound.SC3.UGen.Math       as SCMath
import qualified Sound.SC3.UGen.Bindings.DB as SCEnv
import qualified Sound.SC3.UGen.Envelope.Construct as SCEnvCons

import Sound.SC3.UGen.Enum (DoneAction(PauseSynth), {- EnvCurve(EnvSin,EnvExp) -} )
import Sound.SC3.UGen.Rate (Rate(KR,AR))

import qualified Sound.SC3.UGen.Type as SCType
import Sound.SC3.UGen.Type (UGen)

import qualified Haskore.Interface.SuperCollider.Example  as Example
import qualified Haskore.Interface.SuperCollider.Play     as Play
import qualified Haskore.Interface.SuperCollider.Schedule as Schedule
import qualified Haskore.Interface.SuperCollider.SoundMap as SoundMap

import qualified Haskore.Interface.SuperCollider.Schedule.Channel as CSchedule

import Haskore.Interface.SuperCollider.Schedule.Channel
   ({- rhythmicMusicFromDrum, -} rhythmicMusicFromMelody,
    ugenFromSound, installSound)

import           Haskore.Melody         as Melody
import qualified Haskore.Music          as Music
import           Haskore.Music.Rhythmic as RhyMusic

-- import qualified Haskore.Composition.Drum   as Drum
-- import           Haskore.Composition.Chord as Chord
import           Haskore.Basic.Pitch    as Pitch
import qualified Haskore.Basic.Duration as Dur

import System.Random (randomRs, mkStdGen)

import Data.List (genericLength)



{-
blub :: UGen -> SoundMap.Instrument
blub pan =
  SoundMap.instrumentFromUGen $ \ vel freq ->
     let dur = 0.3
         env = SCEnv.envGen KR 1 1 0 1 PauseSynth
                     (SCEnvCons.envSine dur 1)
     in  0.2 * env * SCType.mce [1+pan,1-pan] *
         SCFilt.hpf
            (SCOsci.pulse AR (SCType.mce [freq*0.995, freq*1.005]) 0.5)
            (4000 * 0.5**env)
-}


zirp :: UGen -> SoundMap.Instrument
zirp pan =
  SoundMap.instrumentFromUGen $ \ vel freq ->
     let env = SCEnv.envGen KR 1 1 0 1 PauseSynth $
                   (SCEnvCons.envSine 0.2 1)
{-
                   SCEnvCons.env
                      [0, 1, 0.1]
                      [0.1, 0.5]
                      [EnvSin, EnvExp]
                      (-1) (-1)
-}
     in  vel * env * SCType.mce [1+pan,1-pan] *
         SCFilt.hpf
            (SCOsci.saw AR (SCType.mce [freq*0.995, freq*1.005]))
            (2000 * 0.5**env)


-- cf. Example.Air
sliceVertInf :: Int -> [a] -> [[a]]
sliceVertInf n =
   map (take n) . iterate (drop n)

-- properFraction is useless for negative numbers
splitFraction :: (RealFrac a) => a -> (Int, a)
splitFraction x =
   let n = floor x
   in  (n, x - fromIntegral n)

makeChord ::
   [Pitch.Octave -> Music.Dur -> attr -> Music.T note]
    -> attr -> Double -> Music.T note
makeChord ns pan x =
   let (oct,p) = splitFraction x
       (high,low) = splitAt (floor (p*genericLength ns)) ns
   in  chord $ map (\n -> n sn pan) $
       map ($ oct) low ++ map ($ (oct+1)) high

zirpScale :: Melody.T Double
zirpScale =
   let binomDists = map sum $ sliceVertInf 3 $
          randomRs (-1,1::Double) (mkStdGen 102)
       panoramas = randomRs (-0.5,0.5) (mkStdGen 95)
   in  Music.legato hn $ Music.line $
       zipWith3 makeChord
          (concat $ map (replicate 16)
             [[c, e, a], [c, e, g], [d, fs, a], [c, e, a]])
          panoramas $
       zipWith (+) binomDists $
       iterate (0.02+) 0.2



bassSquare :: SoundMap.Instrument
bassSquare =
  SoundMap.instrumentFromUGen $ \ vel freq ->
     vel * SCOsci.pulse AR (SCType.mce [freq*0.999, freq*1.001])
            (0.5 + 0.3 * SCOsci.sinOsc KR 0.3 0)

infix 7 +:

(+:) :: Music.T note -> Music.T note -> Music.T note
m0 +: m1 =
   Music.take (Music.dur m0 - Music.dur m1) m0 +:+ m1

bassMelody :: Melody.T ()
bassMelody =
   Music.transpose (-24) $
   Music.line $
--   a 0 wn () : c 1 wn () : d 1 wn () : a 1 wn () : []
   a 0 wn () +: (g 0 sn () +:+ a 0 en ()) :
   c 1 wn () +: (a 0 sn () +:+ c 1 en ()) :
   d 1 wn () +: (c 1 sn () +:+ d 1 en () +:+ c 1 en ()) :
   a 1 wn () : []


tempo :: Dur.Ratio
tempo = 2


schedule :: Schedule.T
schedule =
   CSchedule.fromRhythmicMusic $
   do zirpInstr <- installSound SoundMap.with1Attribute "zirp" zirp
      bassInstr <- installSound SoundMap.with0Attributes "bass" bassSquare
      hihatDrum <- installSound SoundMap.with0Attributes "hihat" Example.hihat
      let mix = -- flip SCMath.clip2 1 $
            0.2 * (
--               ugenFromSound zirpInstr +
--               (let z = ugenFromSound zirpInstr
--                in  z + SCFilt.combN z 0.11 0.11 2) +
               (Example.reverb 4 (ugenFromSound zirpInstr)) +
               0.4*(flip (SCFilt.rlpf (ugenFromSound bassInstr)) 0.1 $
                    flip SCFilt.lag 1 $
                    400 * 8 ** SCOsci.lfSaw KR (Dur.toNumber tempo / 4) 1) +
               5 * ugenFromSound hihatDrum)
      return (mix,
         Music.changeTempo tempo $
--         Music.take 10 $
--         Music.drop (4*4*4*dhn) $
         (rhythmicMusicFromMelody zirpInstr zirpScale =:=
          Music.rest (sn/2) +:+
           (-- Music.replicate (4*4) (rhythmicMusicFromDrum hihatDrum qn) =:=
             rhythmicMusicFromMelody bassInstr bassMelody)) +:+
          Music.rest wn
{-
           Music.transpose (12) (
              Chord.harmonicGen () (qn) [e 0, a 0, c 1] +:+
              Chord.harmonicGen () (qn) [g 0, c 1, e 1] +:+
              Chord.harmonicGen () (qn) [a 0, d 1, fs 1] +:+
              Chord.harmonicGen () (qn) [c 1, e 1, a 1])
-}
              )


main :: IO ()
main = Play.schedule Play.defaultLatency schedule
