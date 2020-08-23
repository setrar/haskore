module Haskore.Interface.SuperCollider.Example where

import qualified Haskore.Interface.SuperCollider.Play     as Play
import qualified Haskore.Interface.SuperCollider.Render   as Render
import qualified Haskore.Interface.SuperCollider.Schedule as Schedule
import qualified Haskore.Interface.SuperCollider.SoundMap as SoundMap

import qualified Sound.SC3.UGen.UGen       as SCUGen
import qualified Sound.SC3.UGen.Math       as SCMath
import qualified Sound.SC3.UGen.Rate       as SCRate
import qualified Sound.SC3.UGen.Oscillator as SCOsci
import qualified Sound.SC3.UGen.Filter     as SCFilter
import qualified Sound.SC3.UGen.Noise.Base as SCNoise
import qualified Sound.SC3.UGen.Envelope   as SCEnv
import qualified Sound.SC3.UGen.Envelope.Construct as SCEnvCons
import Sound.SC3.UGen.Enum
           (DoneAction(DoNothing, PauseSynth),
            EnvCurve(EnvLin, EnvSin))

import           Sound.SC3.UGen.UGen (UGen)

import           Haskore.Melody.Standard   as StdMelody
import qualified Haskore.Melody            as Melody
import           Haskore.Music.Rhythmic    as RhyMusic
import qualified Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music             as Music

import qualified Haskore.Example.ChildSong6 as ChildSong6
import qualified Haskore.Example.Kantate147 as Kantate147

import qualified Haskore.Composition.Drum   as Drum
import qualified Haskore.Composition.Chord  as Chord
import qualified Haskore.Composition.Rhythm as Rhythm

import qualified Data.ByteString.Lazy as B

import Data.List (genericLength)
import Data.Ratio(Rational,(%))
import System.Random (randomRs, mkStdGen)


latency :: Double
latency = 0.1

type Song = (String, Schedule.T)

play :: Song -> IO ()
play =
   Play.schedule latency . snd

render :: Song -> IO ()
render (fileName, schedule) =
   do B.writeFile (fileName++".osc") (Render.byteStringFromSchedule schedule)
      Render.runSCSynth [] 2 Render.AIFF Render.Int16 44100 fileName
      return ()



glissandoStringsUGen :: UGen -> UGen -> UGen
glissandoStringsUGen vel freq =
    let -- control = 2*freq - SCFilter.lag freq 0.2
        control = SCFilter.lag freq 0.2
        volume  = vel * 0.3
        tone0   = SCOsci.saw SCRate.AR (control * SCUGen.MCE [0.998, 1.005])
        tone1   = SCOsci.saw SCRate.AR (control * SCUGen.MCE [0.995, 1.002])
    in  (tone0 + tone1) * volume

reverb :: UGen -> UGen -> UGen
reverb dec x =
   0.7*x + 0.03 *
      sum (map
         (\delfl -> let del = SCUGen.Constant delfl
                    in  SCFilter.combN x del del dec)
         (take 8 (randomRs (0.01,0.03) (mkStdGen 2347))))

filterStrings :: SoundMap.Instrument
filterStrings =
  SoundMap.instrumentFromUGen $ \ vel freq ->
   let lfo = SCOsci.sinOsc SCRate.KR 0.1 0 * 1500 + 2000
       sweep = SCFilter.rlpf (glissandoStringsUGen vel freq) lfo 0.1
   in  reverb 3 sweep


glissando :: Song
glissando =
   ("Glissando",
    Schedule.fromMelody filterStrings $
       changeTempo 3 $ transpose (-48) $ ChildSong6.mainVoice)


-- | normalize a list of numbers, such that they have a specific average
normalizeLevel :: Fractional a => a -> [a] -> [a]
normalizeLevel newAvrg xs =
   let avrg = sum xs / genericLength xs
   in  map ((newAvrg-avrg)+) xs

strings :: SoundMap.Instrument
strings =
  SoundMap.instrumentFromUGen stringsUGen

stringsUGen :: UGen -> UGen -> UGen
stringsUGen vel freq =
    let n = 5
        volume = vel * 0.5 / sqrt (fromIntegral n)
        detunes =
           normalizeLevel 1 $ take (2*n) $
              randomRs (0,0.03) $ mkStdGen 912
        phases =
           randomRs (0,2) $ mkStdGen 54
        tones =
           zipWith
              (\detune phase -> SCOsci.lfSaw SCRate.AR (freq*detune) phase)
              detunes phases
        (tonesLeft,tonesRight) = splitAt n tones
    in  volume * SCUGen.MCE [sum tonesLeft, sum tonesRight]

modulatedStrings :: SoundMap.Instrument
modulatedStrings =
  SoundMap.instrumentFromUGen $ \ vel freq ->
    let n = 3
        volume = vel * 0.3 / sqrt (fromIntegral n)
        tones =
           zipWith3
              (\depth rate phase ->
                   SCOsci.saw SCRate.AR
                      (freq * (1 + depth * SCOsci.sinOsc SCRate.KR rate phase)))
              (randomRs (0,0.015) (mkStdGen 912))
              (randomRs (2,5)     (mkStdGen 105))
              (randomRs (0,2*pi)  (mkStdGen 234))
        (tonesLeft,tonesRight) = splitAt n $ take (2*n) tones
    in  volume * SCUGen.MCE [sum tonesLeft, sum tonesRight]

brass :: SoundMap.Instrument
brass =
  SoundMap.instrumentFromUGen $ \ vel freq ->
    let volume  = vel * 0.3
        detune  = SCEnv.xLine SCRate.KR 10 0.00001 2 PauseSynth
        tone0   = SCOsci.saw SCRate.AR (freq*(1-0.004*detune)) * volume
        tone1   = SCOsci.saw SCRate.AR (freq*(1-0.001*detune)) * volume
        tone2   = SCOsci.saw SCRate.AR (freq*(1+0.005*detune)) * volume
    in  tone0 + tone1 + tone2

sawPerc :: SoundMap.Instrument
sawPerc = SoundMap.instrumentFromUGen sawPercUGen

sawPercUGen :: UGen -> UGen -> UGen
sawPercUGen vel freq =
    let -- env = SCEnv.envGen SCRate.KR 1 2 0 1 PauseSynth SCEnv.envperc'
        env = SCEnv.xLine SCRate.KR 1.5 0.1 1 PauseSynth
        saw = vel * SCEnv.xLine SCRate.KR 1 0.05 5 DoNothing
              * SCOsci.saw SCRate.AR freq
        prc = SCFilter.rlpf saw (exp env * freq * 1.2) 0.05
    in  prc * 0.3

dynPerc :: UGen -> SoundMap.Instrument
dynPerc detune = SoundMap.instrumentFromUGen (dynPercUGen detune)

dynPercUGen :: UGen -> UGen -> UGen -> UGen
dynPercUGen detune vel freq =
    let env = SCEnv.xLine SCRate.KR 1.5 0.1 1 DoNothing
        saw = SCEnv.xLine SCRate.KR 1 0.05 5 DoNothing
               * SCOsci.saw SCRate.AR
                    (SCUGen.MCE [freq*(1-detune), freq*(1+detune)])
        prc = SCFilter.rlpf saw (exp (2*vel*env) * freq * 1.2) 0.05
    in  prc * 0.1

fmBass :: UGen -> SoundMap.Instrument
fmBass ratio =
  SoundMap.instrumentFromUGen $ \ vel freq ->
    let env = SCEnv.xLine SCRate.KR 1 0.01 1 DoNothing
        modOsci = SCOsci.sinOsc SCRate.AR (ratio*freq) 0 * env * vel * 10
        osci    = SCOsci.sinOsc SCRate.AR freq modOsci *env
    in  osci * 1

fmGuitar :: UGen -> SoundMap.Instrument
fmGuitar ratio =
  SoundMap.instrumentFromUGen $ \ vel freq ->
   let modEnv  = SCEnv.xLine SCRate.KR 0.5 2 5 DoNothing
       modOsci = SCOsci.sinOsc SCRate.AR (ratio*freq) 0 * modEnv * vel * 10
       env  = SCEnv.xLine SCRate.KR 1 0.01 5 DoNothing
       lfo  = SCOsci.sinOsc SCRate.KR 3 0 * 10
       osci = SCOsci.sinOsc SCRate.AR (freq+lfo) modOsci * env
   in  osci


xylophone :: SoundMap.Instrument
xylophone =
  SoundMap.instrumentFromUGen $ \ vel freq ->
   let env1  = SCEnv.xLine SCRate.KR 1.5 0.03 0.2 PauseSynth
       sine1 = SCOsci.sinOsc SCRate.AR freq 0 * env1
       env2  = SCEnv.xLine SCRate.KR 1.5 0.01 0.2 PauseSynth
       sine2 = SCOsci.sinOsc SCRate.AR (3*freq) 0 * env2 * vel
   in  sine1 + sine2


harpsichord :: SoundMap.Instrument
harpsichord =
  SoundMap.instrumentFromUGen $ \ vel freq ->
   let env = SCEnv.xLine SCRate.KR 1.5 0.03 1 PauseSynth
       saw = SCOsci.saw SCRate.AR (SCUGen.MCE [freq*0.995, freq*1.005]) * env * vel
   in  saw


{- |
The 'dust' UGen as provided by SuperCollider has a problem:
The higher the sample rate, the more narrow are the impulses,
the less volume you obtain.
We work-around this by choosing a larger impulse.
(We could also use a fixed width for the impulses.)
-}
colorDust :: Double -> SoundMap.Instrument
colorDust sampleRate =
  SoundMap.instrumentFromUGen $ \ _vel freq ->
    0.0005 * SCFilter.rlpf
      (SCUGen.Constant sampleRate * SCNoise.dust zeroUId SCRate.AR 10)
      freq 0.01


flangerSquare :: UGen -> UGen
flangerSquare freq =
   SCOsci.pulse SCRate.AR freq (0.5 + 0.3 * SCOsci.sinOsc SCRate.KR 0.5 0)

cubicSine :: UGen -> UGen
cubicSine freq =
   let osci = SCOsci.sinOsc SCRate.AR (SCUGen.MCE [freq*1.003, freq*0.997]) 0
   in  osci*osci*osci

cubicPhaserSaw :: UGen -> UGen
cubicPhaserSaw freq =
   let osci dfreq = sum $ map (SCOsci.saw SCRate.AR) [dfreq*1.003, dfreq*0.997]
       osciCubic dfreq = osci dfreq * osci dfreq * osci dfreq
   in  SCUGen.MCE [osciCubic (freq*1.002), osciCubic (freq*0.998)]





midiInstrMap :: SoundMap.InstrumentTable MidiMusic.Instr
midiInstrMap =
  [(MidiMusic.AcousticGrandPiano, sawPerc),
   (MidiMusic.SynthBass1,         dynPerc (SCUGen.Constant 0.003)),
   (MidiMusic.SynthBrass1,        brass),
   (MidiMusic.StringEnsemble1,    strings)]


childSong :: Song
childSong =
   ("ChildSong",
    Schedule.fromMusic
      (Schedule.rhythmicMusic [] midiInstrMap)
      -- (changeTempo 3 (transpose (-48) (MidiMusic.fromStdMelody MidiMusic.AcousticGrandPiano ChildSong6.mainVoice)))
      (loudness 0.5 $ changeTempo 1 $ transpose 0 ChildSong6.song))

bassMusic :: MidiMusic.T
bassMusic =
   MidiMusic.fromStdMelody MidiMusic.SynthBass1
      (Music.line (zipWith (\n v -> n tn [Velocity (v%1000)])
         (cycle [c 0, d 0, g 0, d 0])
         (randomRs (0,1000) (mkStdGen 142))))

bass :: Song
bass =
   ("BassPercs",
    Schedule.fromMusic
       (Schedule.rhythmicMusic [] midiInstrMap)
       bassMusic)

bassFinite :: Song
bassFinite =
   ("BassPercs",
    Schedule.fromMusic (Schedule.rhythmicMusic [] midiInstrMap) $
    Music.take 4 bassMusic)


{- |
Oscillator with the waveform of a parabola.

The parabola is created by moving a saw up or down, and squaring it.
That is we repeat the function @\x -> (x+s)^2@.

The crux is the subsequent normalization to zero DC offset.
This boils down to a parabola plus a saw.
-}
parabolaOsci ::
      UGen {- ^ Zero means a perfect parabola.
                The higher the absolute value, the closer is the waveform to a saw. -}
   -> UGen {- ^ frequency -}
   -> UGen
parabolaOsci straight freq =
   let x = SCOsci.saw SCRate.AR freq
       {-
       integrate parabola, average and subtract offset
       (1+s)^3-(-1+s)^3)/3 = (3*s^2+1)*2/3
       
       (x+s)^2 - (3*s^2+1)*2/(3*2)
        = (x+s)^2 - (3*s^2+1)/3
        = x^2 + 2*s*x - 1/3
       -}
   in  (x+2*straight) * x - 1/3



kantate147 :: Song
kantate147 =
   ("Kantate147",
    Schedule.fromMusicGlobalEffect
       (Schedule.rhythmicMusic [] midiInstrMap)
       (\sig ->
          flip SCMath.clip2 1 $
          0.5*sig + 0.2*SCFilter.combN sig 0.2 0.2 5, 2) $
    MidiMusic.fromMelodyIgnoreAttr MidiMusic.StringEnsemble1 $
    loudness 0.5 $ transpose (-12) $
    changeTempo 2 Kantate147.song)


-------------------------------------------------------------------



pad :: SoundMap.Instrument
pad =
  SoundMap.withDuration $ \ dur ->
  SoundMap.instrumentFromUGen $ \ vel freq ->
    let filtered   = SCFilter.bpf mix filterFreq 0.3
        filterFreq = SCEnv.xLine SCRate.KR 100 4000 totalDur DoNothing
        totalDur = 3 + dur
        mix     = (tone0 + tone1) * env * vel * 0.2
        env     = SCEnv.envGen SCRate.KR 1 2 0 1 PauseSynth
                     (SCEnvCons.envSine totalDur 1)
                     -- (bridge 1 dur 1)
        tone0   = SCOsci.saw SCRate.AR (freq * SCUGen.MCE [0.998, 1.005])
        tone1   = SCOsci.saw SCRate.AR (freq * SCUGen.MCE [0.995, 1.002])
    in  filtered

bridge :: UGen -> UGen -> UGen -> [UGen]
bridge fadeDur sustainDur lvl =
   let segTypes = [EnvSin, EnvLin, EnvSin]
       clippedFadeDur = min fadeDur sustainDur
   in  SCEnvCons.env
          [0, lvl, lvl, 0]
          [clippedFadeDur, max 0 (sustainDur-fadeDur), clippedFadeDur]
          segTypes (-1) (-1)

zeroUId :: SCUGen.UGenId
zeroUId = SCUGen.UGenId 0

hihat :: SoundMap.Drum
hihat =
  SoundMap.drumFromUGen $ \ vel ->
   let noise = SCNoise.whiteNoise zeroUId SCRate.AR
   in  SCFilter.hpf noise (SCUGen.MCE [4000, 4001]) *
       SCEnv.xLine SCRate.KR 1 0.01 0.2 DoNothing * vel * 0.25

bassdrum :: SoundMap.Drum
bassdrum =
  SoundMap.drumFromUGen $ \ vel ->
   let freq = SCEnv.xLine SCRate.KR 100 10 0.5 DoNothing
       env  = SCEnv.xLine SCRate.KR 1 0.1 0.5 DoNothing * vel * 0.8
       osci = SCOsci.sinOsc SCRate.AR freq 0 * SCUGen.MCE [env,env]
   in  SCMath.clip2 0.5 osci




data Instrument =
     SynthBass Double
   | Pad
   deriving (Eq, Ord)

data Drum =
     Hihat
   | BassDrum
   deriving (Show, Eq, Ord)


instrMap :: SoundMap.InstrumentTableWithAttributes Instrument
instrMap =
   SoundMap.assign1 "synthbass"
      (\x -> do SynthBass detune <- Just x; Just detune)
--      (\x -> case x of SynthBass detune -> Just detune; _ -> Nothing)
      dynPerc :
   SoundMap.assign "pad"
      (\x -> do Pad              <- Just x; Just ())
      pad :
   []

drumMap :: SoundMap.DrumTableWithAttributes Drum
drumMap =
   SoundMap.assignEq "hihat" Hihat hihat :
   SoundMap.assignEq "bassdrum" BassDrum bassdrum :
   []


synthBassAttrsToInstr :: (Rational,Double) -> (Rational, Instrument)
synthBassAttrsToInstr (vel,detune) = (vel, SynthBass detune)

synthBassMelody :: Melody.T (Rational,Double)
synthBassMelody =
   Music.line (zipWith (\n (vel,detune) -> n tn (vel%1000,detune))
      (cycle [c 0, d 0, g 0, d 0])
      (zip (randomRs (0,1000)     (mkStdGen 142))
           (randomRs (-0.02,0.02) (mkStdGen 857))))

padAttrsToInstr :: () -> (Rational, Instrument)
padAttrsToInstr _ = (1, Pad)

padMelody :: Melody.T ()
padMelody =
   Music.line $ cycle $
      map (\(dur, chd) -> Chord.harmonicGen () dur chd)
          [(hn, [c 1, e 1, a 1]),
           (hn, [b 0, e 1, g 1]),
           (hn, [a 0, d 1, f 1]),
           (hn, [b 0, d 1, g 1])]

rhythm :: RhyMusic.T Drum Instrument
rhythm =
   Music.repeat (Drum.toMusic Hihat tn []) =:=
   Music.repeat (Rhythm.toMusicWithDrumUnit tn BassDrum
      (Rhythm.fromString "x..x..x..x..x..."))


chillOutMusic :: RhyMusic.T Drum Instrument
chillOutMusic =
   RhyMusic.changeTempo 0.5 $
   RhyMusic.fromMelody synthBassAttrsToInstr synthBassMelody =:=
   RhyMusic.fromMelody padAttrsToInstr padMelody =:=
   rhythm


chillOut :: Song
chillOut =
   ("ChillOut",
    Schedule.fromMusic
      (Schedule.rhythmicMusicWithAttributes drumMap instrMap)
      chillOutMusic)

chillOutFinite :: Song
chillOutFinite =
   ("ChillOut",
    Schedule.fromMusicGlobalEffect
      (Schedule.rhythmicMusicWithAttributes drumMap instrMap)
      (flip SCMath.clip2 1, 2) $
      Music.take 4 chillOutMusic)




data InstrumentTriplet =
     SawPerc
   | DynPerc Double
   deriving (Eq, Ord)


sawPercAssign :: SoundMap.InstrumentAssign InstrumentTriplet
sawPercAssign =
   SoundMap.assign "sawPerc"
      (\x -> do SawPerc <- Just x; Just ())
      sawPerc

dynPercAssign :: SoundMap.InstrumentAssign InstrumentTriplet
dynPercAssign =
   SoundMap.assign1 "dynPerc"
      (\x -> do DynPerc detune <- Just x; Just detune)
      dynPerc



tripletsMusic :: RhyMusic.T Drum InstrumentTriplet
tripletsMusic =
   let mainMel =
          RhyMusic.fromMelody (\() -> (1, SawPerc)) $
          Music.transpose 12 $ Music.line $
          cycle [c 0 qn (), b 0 qn (), c 1 qn ()]
       bassMel =
          RhyMusic.fromMelody (\detune -> (1, DynPerc detune)) $
          Music.line $
          cycle [c 0 qn 0.001, c 0 qn 0.003, c 0 qn 0.01]
   in  Music.changeTempo 2 $
       Music.chord [Music.changeTempo 3 mainMel, bassMel]


triplets :: Song
triplets =
   ("Triplets",
    Schedule.fromRhythmicMusicSoundEffects
       (SoundMap.registerInstrument dynPercAssign $ \dynPercUG ->
        SoundMap.registerInstrument sawPercAssign $ \sawPercUG ->
          let lfoSine   = exp (SCOsci.sinOsc SCRate.KR 0.2 (-pi/2) * 0.5) * 1000
              lfoSquare = exp (SCOsci.pulse SCRate.KR 5.1 0.5 * 1) * 1000
              mix =
                 SCFilter.rlpf (0.5 * sawPercUG) lfoSine 0.1 +
                 SCFilter.rlpf (0.5 * dynPercUG) lfoSquare 0.1
          in  SoundMap.soundEffect mix)
       2 tripletsMusic)
