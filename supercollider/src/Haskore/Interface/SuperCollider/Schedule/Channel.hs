{- |
This is a variant of the "Haskore.Interface.SuperCollider.Schedule.Install" module.
It assigns output channels to instruments
such that instrument specific global effects can be applied to them.
-}
module Haskore.Interface.SuperCollider.Schedule.Channel where

import qualified Sound.SC3.UGen.Bindings.DB as SCOsci
import qualified Sound.SC3.UGen.Bindings.DB as SCFilt

import Sound.SC3.UGen.Rate (Rate(KR))

import qualified Sound.SC3.Server.PlayEasy  as SCPlay
import qualified Sound.SC3.Server.Command   as SCCmd
import Sound.SC3.Server.Enum (AddAction(AddToTail))

import Sound.SC3.UGen.Type (UGen)

import qualified Sound.OSC.Type as OSC

import qualified Haskore.Interface.SuperCollider.Channel as Channel
import Haskore.Interface.SuperCollider.Channel (Channel, NumberChannels)

import qualified Haskore.Interface.SuperCollider.Example       as Example
import qualified Haskore.Interface.SuperCollider.Play          as Play
import qualified Haskore.Interface.SuperCollider.Schedule      as Schedule
import qualified Haskore.Interface.SuperCollider.Note          as Note
import qualified Haskore.Interface.SuperCollider.Performance   as SCPf
import qualified Haskore.Interface.SuperCollider.SoundMap      as SoundMap

import Haskore.Interface.SuperCollider.SoundMap
         (InstrumentParameters, DrumParameters, AttributeList)

import qualified Haskore.Composition.Drum   as Drum
import qualified Haskore.Composition.Rhythm as Rhythm

import qualified Haskore.Music          as Music
import           Haskore.Music (mapNote)
import qualified Haskore.Music.Rhythmic as RhyMusic
import           Haskore.Music.Rhythmic (qn)
import           Haskore.Melody         as Melody

import qualified Haskore.General.IdGenerator      as IdGen

import Control.Monad.Trans.State  (StateT, runStateT, get, put, )
import Control.Monad.Trans.Writer (Writer, tell, runWriter, )
import Control.Monad.Trans.Class (lift, )
import Control.Monad (liftM2, liftM, ap, )
import Control.Applicative (Applicative, pure, (<*>), )



{- * Install instruments -}


newtype Environment a =
   Environment {unwrapEnvironment ::
      StateT (Channel, NumberChannels) (Writer [OSC.Message]) a}

instance Functor Environment where
   fmap = liftM

instance Applicative Environment where
   (<*>) = ap
   pure = return

instance Monad Environment where
   x >>= y  = Environment $ unwrapEnvironment . y =<< unwrapEnvironment x
   return   = Environment . return

writeOSC :: [OSC.Message] -> Environment ()
writeOSC = Environment . lift . tell

nextChannel :: NumberChannels -> Environment Channel
nextChannel numChan =
   Environment $
      do (chan, maxNumChan) <- get
         put (chan+numChan, max numChan maxNumChan)
         return chan


data Sound params attr =
     Sound String (Channel, NumberChannels) (attr -> AttributeList)

type Instrument attr = Sound InstrumentParameters attr
type Drum       attr = Sound DrumParameters attr


installUGen ::
   String ->
   UGen ->
   Environment (Channel, NumberChannels)
installUGen name sound =
   do let numChan = SCPlay.mceDegree sound
      chan <- nextChannel numChan
      writeOSC [Schedule.installUGenMsg name chan sound]
      return (chan, numChan)

installSound ::
   SoundMap.SoundParameters params =>
   (parameterTuple -> AttributeList, graph -> SoundMap.Sound params) ->
   String ->
   graph ->
   Environment (Sound params parameterTuple)
installSound (makeAttributeList, makeSoundUGen) name instr =
   do chanChunk <-
         installUGen name $ SoundMap.ugenFromSound $ makeSoundUGen instr
      return (Sound name chanChunk makeAttributeList)

ugenFromSound :: Sound params attr -> UGen
ugenFromSound (Sound _ (chan, numChan) _) =
   Channel.readUGen numChan chan





{- * Play music -}

{- |
These types are identically defined to those from
"Haskore.Interface.SuperCollider.Schedule.Install"
but they shall be distinct.
Instruments that are installed for use of global effects
cannot be used with the simple 'playMusic' routine.
-}
data SoundAttributes params = SoundAttributes AttributeList String
   deriving (Eq, Ord)

type DrumAttributes       = SoundAttributes DrumParameters
type InstrumentAttributes = SoundAttributes InstrumentParameters


rhythmicMusicFromDynamicMelody ::
   Instrument attr ->
   Melody.T (Rational, attr) ->
   RhyMusic.T DrumAttributes InstrumentAttributes
rhythmicMusicFromDynamicMelody (Sound name _ flattenAttr) =
   RhyMusic.fromMelody
      (\(vel,attr) -> (vel, SoundAttributes (flattenAttr attr) name))

rhythmicMusicFromMelody ::
   Instrument attr ->
   Melody.T attr ->
   RhyMusic.T DrumAttributes InstrumentAttributes
rhythmicMusicFromMelody instr =
   rhythmicMusicFromDynamicMelody instr .
   mapNote (\(Melody.Note attr p) -> Melody.Note (1,attr) p)


rhythmicMusicFromRhythm ::
   Drum () ->
   Music.Dur ->
   String ->
   RhyMusic.T DrumAttributes InstrumentAttributes
rhythmicMusicFromRhythm (Sound name _ _) dur =
   Rhythm.toMusicWithDrumUnit dur (SoundAttributes [] name) .
   Rhythm.fromString

rhythmicMusicFromDrum ::
   Drum () ->
   Music.Dur ->
   RhyMusic.T DrumAttributes InstrumentAttributes
rhythmicMusicFromDrum (Sound name _ _) dur =
   Drum.toMusicDefaultAttr (SoundAttributes [] name) dur


{-
reset :: Environment ()
reset =
   do Channel.reset ChannelState.manager
      lift SCPlay.reset
-}

fromRhythmicMusic ::
   Environment
      (UGen,
       RhyMusic.T DrumAttributes InstrumentAttributes) ->
   Schedule.T
fromRhythmicMusic genMusic =
   let {- a nice loop in order to determine the maximum number of channels needed
          and reserving an according number of channels beginning at zero -}
       (((effect,song), (_,maxNumChan)), installSounds) =
          runWriter $
             runStateT (unwrapEnvironment genMusic) (maxNumChan,0)
       (sid,pf) =
          SCPf.fixNodeIds $
          liftM2 (,)
             IdGen.alloc
             (SCPf.fromMusic
                 (Note.fromRhythmicNoteWithAttributes
                    (\(SoundAttributes params name) -> (params,name))
                    (\(SoundAttributes params name) -> (params,name)))
                 song)
       effectsName = "global effects"
   in  {- We rely on the fact, that the performance player
          always adds new nodes to the head.
          This way, the effect is run after the instrument nodes. -}
       Schedule.fromPerformance
          (Schedule.installUGenMsg effectsName Schedule.defaultChannel effect :
           installSounds)
          [SCCmd.s_new effectsName sid AddToTail SCPlay.homeId []]
          pf


{-
run :: Environment UDP a -> IO a
run act =
   SCPlay.withSC3 (evalStateT act Channel.least)

writeScript :: FilePath -> Environment File a -> IO a
writeScript fn act =
   SCPlay.withSC3File fn (evalStateT act Channel.least)
-}


{- * Example music -}

example :: IO ()
example =
   Play.schedule Play.defaultLatency $ fromRhythmicMusic $
   do sawPerc <- installSound SoundMap.with0Attributes "saw percussion" Example.sawPerc
      dynPerc <- installSound SoundMap.with1Attribute  "detuned bass"   Example.dynPerc
      let lfoSine   = exp (SCOsci.sinOsc KR 0.2 (-pi/2) * 0.5) * 1000
          lfoSquare = exp (SCOsci.pulse KR 5.1 0.5 * 1) * 1000
          mix =
            SCFilt.rlpf (0.5 * ugenFromSound sawPerc) lfoSine 0.1 +
            SCFilt.rlpf (0.5 * ugenFromSound dynPerc) lfoSquare 0.1
            -- SCUGen.Constant 0
      let mel = rhythmicMusicFromMelody sawPerc $ Music.transpose 12 $ Music.line $
            cycle [c 0 qn (), b 0 qn (), c 1 qn ()]
          bass = rhythmicMusicFromMelody dynPerc $ Music.line $
            cycle [c 0 qn 0.001, c 0 qn 0.003, c 0 qn 0.01]
      return (mix,
         -- (0.3 * SCOsci.sinOsc AR 880 0) $
         Music.changeTempo 2 $
         Music.chord [Music.changeTempo 3 mel, bass])
