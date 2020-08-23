{- |
This is a module specialised to life performances.

You can create and install instruments in SuperCollider,
obtain a handle and use that in your song.

The alternative way would be to upload an instrument
whenever one tone shall be played.
This is less efficient but simpler and certainly more flexible.

It is certainly not sensible to import that module.
Maybe I should provide it as Extra-Source-File or
as Main module of a dummy executable in order to ship it via Cabal.
-}
module Haskore.Interface.SuperCollider.Play.Live where

import qualified Sound.SC3 as SC
import qualified Sound.SC3.UGen.UGen as SCUGen
import qualified Sound.SC3.UGen.Bindings.DB as SCIO
import qualified Sound.SC3.UGen.Bindings.DB as SCOsci
import qualified Sound.SC3.UGen.Bindings.DB as SCFilt
import qualified Sound.SC3.UGen.Bindings.DB as SCNoise
import qualified Sound.SC3.UGen.Envelope   as SCUGEnv
import qualified Sound.SC3.UGen.UId as SCUId

import Sound.SC3.UGen.Enum (DoneAction(DoNothing, PauseSynth))
import Sound.SC3.UGen.Rate (Rate(AR,KR))

import qualified Sound.SC3.Server.PlayEasy  as SCPlay
import qualified Sound.SC3.Server.Command   as SCCmd
import Sound.SC3.Server.Enum (AddAction(AddToTail, AddToHead))

import Sound.SC3.UGen.Type (UGen, constant, )

import Sound.OSC.Transport.FD (Transport)
import qualified Sound.OSC.Transport.Monad as Trans

import qualified Haskore.Interface.SuperCollider.Example       as Example
import qualified Haskore.Interface.SuperCollider.Play.Install  as IPlay
import qualified Haskore.Interface.SuperCollider.Play          as Play
import qualified Haskore.Interface.SuperCollider.Schedule      as Schedule
import qualified Haskore.Interface.SuperCollider.Schedule.Channel as CSched
import qualified Haskore.Interface.SuperCollider.Schedule.Install as ISched
import qualified Haskore.Interface.SuperCollider.Performance   as SCPf
import qualified Haskore.Interface.SuperCollider.SoundMap      as SoundMap

import Haskore.Interface.SuperCollider.SoundMap
         (InstrumentParameters, DrumParameters, AttributeList)

import qualified Haskore.Interface.SuperCollider.Channel.Env as ChannelEnv
import qualified Haskore.Interface.SuperCollider.Channel as Channel
import Haskore.Interface.SuperCollider.Channel (Channel, NumberChannels)

import Haskore.Interface.SuperCollider.Schedule.Channel
         (Sound(..), ugenFromSound,
          Instrument, -- InstrumentAttributes,
          Drum, -- DrumAttributes,
          rhythmicMusicFromDynamicMelody, rhythmicMusicFromMelody,
          rhythmicMusicFromRhythm, rhythmicMusicFromDrum)
import Haskore.Interface.SuperCollider.Schedule.Install
         (InstrumentAttributes(..), DrumAttributes(..))

import qualified Haskore.Basic.Pitch as Pitch
import qualified Haskore.Basic.Duration as Dur
import           Haskore.Basic.Duration ((%+))

import qualified Haskore.Interface.SuperCollider.Timer as SCTimer

import qualified Haskore.General.IdGenerator      as IdGen

import qualified Haskore.Composition.Drum  as Drum
import           Haskore.Composition.Chord as Chord

import           Haskore.Melody         as Melody
import qualified Haskore.Music          as Music
import           Haskore.Music (mapNote)
import qualified Haskore.Music.Rhythmic as RhyMusic
import           Haskore.Music.Rhythmic (
          (+:+), (=:=),
          wn, hn, qn, en, sn, tn, sfn,
          dwn, dhn, dqn, den, dsn, dtn,
          wnr, hnr, qnr, enr, snr, tnr, sfnr,
          dwnr, dhnr, dqnr, denr, dsnr, dtnr,)

-- import System.Random (randomRs, mkStdGen)

import System.Posix.Process (forkProcess)
-- import Control.Concurrent (forkIO)
import Control.Monad (liftM2)

import Data.List (elemIndex)



{- * Install instruments -}

type InstrumentUGen = UGen -> UGen -> UGen

installInstr ::
   (parameterTuple -> AttributeList, graph -> InstrumentUGen) ->
   String ->
   graph ->
   IO (ISched.Instrument parameterTuple)
installInstr (makeAttributeList, makeSoundUGen) name sound =
   SCPlay.withSC3 $
   IPlay.installSound
      (makeAttributeList, SoundMap.instrumentFromUGen . makeSoundUGen) name sound


installInstr0 ::
   String ->
   InstrumentUGen ->
   IO (ISched.Instrument ())
installInstr0 =
   installInstr SoundMap.with0Attributes

installInstr1 ::
   String ->
   (UGen -> InstrumentUGen) ->
   IO (ISched.Instrument Double)
installInstr1 =
   installInstr SoundMap.with1Attribute

installInstr2 ::
   String ->
   (UGen -> UGen -> InstrumentUGen) ->
   IO (ISched.Instrument (Double,Double))
installInstr2 =
   installInstr SoundMap.with2Attributes


type DrumUGen = UGen -> UGen

installDrum ::
   (parameterTuple -> AttributeList, graph -> DrumUGen) ->
   String ->
   graph ->
   IO (ISched.Drum parameterTuple)
installDrum (makeAttributeList, makeSoundUGen) name sound =
   SCPlay.withSC3 $
   IPlay.installSound
      (makeAttributeList, SoundMap.drumFromUGen . makeSoundUGen) name sound




{- * Install instruments on specific channels -}


installSoundChan ::
   SoundMap.SoundParameters params =>
   String ->
   SoundMap.Sound params ->
   IO (Channel, NumberChannels)
installSoundChan name sound =
   do let ugen = SoundMap.ugenFromSound sound
      let numChan = SCPlay.mceDegree ugen
      chan <- Channel.next ChannelEnv.manager numChan
      SCPlay.withSC3 $
         SCPlay.simpleSync $ SCPlay.d_recv_synthdef name $
         SCIO.out (constant chan) ugen
      return (chan, numChan)


installInstrChan ::
   (parameterTuple -> AttributeList, graph -> InstrumentUGen) ->
   String ->
   graph ->
   IO (Instrument parameterTuple)
installInstrChan (makeAttributeList, makeInstrumentUGen) name sound =
   do chanChunk <-
         installSoundChan name $
         SoundMap.instrumentFromUGen $
         makeInstrumentUGen sound
      return (Sound name chanChunk makeAttributeList)

installDrumChan ::
   (parameterTuple -> AttributeList, graph -> DrumUGen) ->
   String ->
   graph ->
   IO (Drum parameterTuple)
installDrumChan (makeAttributeList, makeDrumUGen) name sound =
   do chanChunk <-
         installSoundChan name $
         SoundMap.drumFromUGen $
         makeDrumUGen sound
      return (Sound name chanChunk makeAttributeList)



{- |
This one is more portable but less elegant.
-}


{- * Play music -}

reset :: IO ()
reset =
   do Channel.reset ChannelEnv.manager
      SCPlay.withSC3 $ SCPlay.reset

playSound :: UGen -> IO ()
playSound = fmap (const ()) . SCPlay.withSC3 . SCPlay.play

playMusic ::
   RhyMusic.T DrumAttributes InstrumentAttributes -> IO ()
playMusic =
   Play.performance Play.defaultLatency [] .
   SCPf.fixNodeIds .
   SCPf.fromRhythmicMusicWithAttributes
      (\(ISched.SoundAttributes params name) -> (params,name))
      (\(ISched.SoundAttributes params name) -> (params,name))

playMusicEffect ::
   UGen ->
   RhyMusic.T CSched.DrumAttributes CSched.InstrumentAttributes ->
   IO ()
playMusicEffect effect song =
   let (sid,pf) =
          SCPf.fixNodeIds $
          liftM2 (,)
             IdGen.alloc
             (SCPf.fromRhythmicMusicWithAttributes
                 (\(CSched.SoundAttributes params name) -> (params,name))
                 (\(CSched.SoundAttributes params name) -> (params,name))
                 song)
       effectsName = "global effects"
   in  SCPlay.withSC3 $
          {- We rely on the fact, that the performance player
             always adds new nodes to the head.
             This way, the effect is run after the instrument nodes. -}
          Play.scheduleWithPlayer
             (Play.messagesGrouped SCTimer.timer Play.defaultLatency)
             (Schedule.fromPerformance
                [Schedule.installUGenMsg effectsName
                    Schedule.defaultChannel effect]
                [SCCmd.s_new effectsName sid AddToTail SCPlay.homeId []]
                pf) >>
          return ()


{- * Interactively play music on keyboard -}

germanLatin1Keyboard :: [Char]
germanLatin1Keyboard =
   let oUmlaut = '\246'
       uUmlaut = '\252'
       szLig   = '\223'
   in  "ysxdcvgbhnjm,l."++oUmlaut:"-q2w3e4rt6z7ui9o0p"++szLig:uUmlaut:"+"

playKey :: ISched.Instrument () -> Char -> IO ()
playKey (ISched.Sound name _) key =
   maybe
      (return ())
      (\pitch ->
          do SCPlay.withSC3 $
                Play.playAtom SCPlay.autoId name $
                   ("pitch", Pitch.intToFreq pitch) :
                   ("velocity", 1) : []
             print (Pitch.fromInt pitch))
      (elemIndex key germanLatin1Keyboard)

{- |
Interprets the keyboard as piano and play according tones,
when keys are hit.

Is it more convenient to have a UGen parameter and install the instrument automatically?
-}
{-
Try suppressing output on keys by
System.IO.hSetEcho System.IO.stdin False
-}
playKeyboard :: ISched.Instrument () -> IO ()
playKeyboard instr =
   let recourse =
          do char <- getChar
             if char == '\004'
               then putStrLn ""
               else do putChar '\008'
                       playKey instr char
                       recourse
   in  recourse



{- * Example music -}


example :: IO ()
example =
   do reset
      sawPerc <- installInstr0 "saw percussion" Example.sawPercUGen
      dynPerc <- installInstr1 "detuned bass"   Example.dynPercUGen
      let mel = ISched.rhythmicMusicFromMelody sawPerc $ Music.transpose 12 $ Music.line
            [c 0 qn (), e 0 qn (),
             Music.chord [c 0 qn (), e 0 qn (), g 0 qn ()]]
          bass = ISched.rhythmicMusicFromMelody dynPerc $ Music.line
            [c 0 qn 0.001, c 0 qn 0.003, c 0 qn 0.01]
      playMusic $
         Music.changeTempo 2 $
         Music.chord [mel,bass]


exampleEffect :: IO ()
exampleEffect =
   do reset
      sawPerc <- installInstrChan SoundMap.with0Attributes "saw percussion" Example.sawPercUGen
      dynPerc <- installInstrChan SoundMap.with1Attribute  "detuned bass"   Example.dynPercUGen
      let lfoSine   = exp (SCOsci.sinOsc KR 0.2 (-pi/2) * 0.5) * 1000
          lfoSquare = exp (SCOsci.pulse KR 5.1 0.5 * 1) * 1000
          mix =
            SCFilt.rlpf (0.5 * ugenFromSound sawPerc) lfoSine 0.1 +
            SCFilt.rlpf (0.5 * ugenFromSound dynPerc) lfoSquare 0.1
            -- SCUGen.Constant 0
      let mel = CSched.rhythmicMusicFromMelody sawPerc $ Music.transpose 12 $ Music.line $
            cycle [c 0 qn (), b 0 qn (), c 1 qn ()]
          bass = CSched.rhythmicMusicFromMelody dynPerc $ Music.line $
            cycle [c 0 qn 0.001, c 0 qn 0.003, c 0 qn 0.01]
      playMusicEffect mix $
         -- (0.3 * SCOsci.sinOsc AR 880 0) $
         Music.changeTempo 2 $
         Music.chord [Music.changeTempo 3 mel, bass]

