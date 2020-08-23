{- |
This is a variant of the "Haskore.Interface.SuperCollider.Play.Install" module.
It assigns output channels to instruments
such that instrument specific global effects can be applied to them.
-}
module Haskore.Interface.SuperCollider.Play.Channel
   {-# DEPRECATED "use Haskore.Interface.SuperCollider.Schedule.Channel instead" #-}
   where

import qualified Sound.SC3.UGen.Bindings.DB as SCOsci
import qualified Sound.SC3.UGen.Bindings.DB as SCFilt

import Sound.SC3.UGen.Rate (Rate(KR))

import qualified Sound.SC3.Server.PlayEasy  as SCPlay
import qualified Sound.SC3.Server.Command   as SCCmd
import Sound.SC3.Server.Enum (AddAction(AddToTail))

import Sound.SC3.UGen.Type (UGen)

import Sound.OSC.Transport.FD (Transport)
import Sound.OSC.Transport.FD.UDP  (UDP)
import qualified Sound.OSC.Transport.File as File

import qualified Haskore.Interface.SuperCollider.Channel.State as ChannelState
import qualified Haskore.Interface.SuperCollider.Channel as Channel
import Haskore.Interface.SuperCollider.Channel (Channel, NumberChannels)

import qualified Haskore.Interface.SuperCollider.Example       as Example
import qualified Haskore.Interface.SuperCollider.Play          as Play
import qualified Haskore.Interface.SuperCollider.Schedule      as Schedule
import qualified Haskore.Interface.SuperCollider.Performance   as SCPf
import qualified Haskore.Interface.SuperCollider.SoundMap      as SoundMap

import Haskore.Interface.SuperCollider.Schedule.Channel
   (Sound(..), SoundAttributes(..), ugenFromSound,
    DrumAttributes, InstrumentAttributes,
    {- rhythmicMusicFromDynamicMelody, -} rhythmicMusicFromMelody,
    {- rhythmicMusicFromRhythm, rhythmicMusicFromDrum, -} )
import Haskore.Interface.SuperCollider.SoundMap (AttributeList)

import qualified Haskore.Music          as Music
import qualified Haskore.Music.Rhythmic as RhyMusic
import           Haskore.Music.Rhythmic (qn)
import           Haskore.Melody         as Melody

import qualified Haskore.Interface.SuperCollider.Timer as SCTimer

import qualified Haskore.General.IdGenerator      as IdGen

import Control.Monad.Trans.State  (StateT, evalStateT, )
import Control.Monad.Trans.Reader (ReaderT, )
import Control.Monad.Trans.Class (lift, )
import Control.Monad (liftM2, )



{- * Install instruments -}


{-
The @[OSC]@ state is used for messages for installing the instruments.
We cannot use a Writer monad for this purpose
because we have to read the generated messages for 'playMusic'.
-}


type Environment t a = StateT Channel (ReaderT t IO) a
-- type Environment t a = StateT Channel (Trans.IO t) a


installUGen :: Transport t =>
   String ->
   UGen ->
   Environment t (Channel, NumberChannels)
installUGen name sound =
   do let numChan = SCPlay.mceDegree sound
      chan <- Channel.next ChannelState.manager numChan
      lift $ SCPlay.simpleSync $
         Schedule.installUGenMsg name chan sound
      return (chan, numChan)

installSound ::
   (Transport t, SoundMap.SoundParameters params) =>
   (parameterTuple -> AttributeList, graph -> SoundMap.Sound params) ->
   String ->
   graph ->
   Environment t (Sound params parameterTuple)
installSound (makeAttributeList, makeSoundUGen) name instr =
   do chanChunk <-
         installUGen name $ SoundMap.ugenFromSound $ makeSoundUGen instr
      return (Sound name chanChunk makeAttributeList)




{- * Play music -}

reset :: Transport t => Environment t ()
reset =
   do Channel.reset ChannelState.manager
      lift SCPlay.reset


playMusic :: Transport t =>
   UGen ->
   RhyMusic.T DrumAttributes InstrumentAttributes ->
   Environment t ()
playMusic effect song =
   let (sid,pf) =
          SCPf.fixNodeIds $
          liftM2 (,)
             IdGen.alloc
             (SCPf.fromRhythmicMusicWithAttributes
                 (\(SoundAttributes attrs name) -> (attrs,name))
                 (\(SoundAttributes attrs name) -> (attrs,name))
                 song)
       effectsName = "global effects"
   in  lift $
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


run :: Environment UDP a -> IO a
run act =
   SCPlay.withSC3 (evalStateT act Channel.least)

writeScript :: FilePath -> Environment File.T a -> IO a
writeScript fn act =
   SCPlay.withSC3File fn (evalStateT act Channel.least)



{- * Example music -}

example :: IO ()
example =
   run $
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
      playMusic mix $
         -- (0.3 * SCOsci.sinOsc AR 880 0) $
         Music.changeTempo 2 $
         Music.chord [Music.changeTempo 3 mel, bass]
