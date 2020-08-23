module Haskore.Interface.SuperCollider.Play (
    music, melody, performance,
    schedule, scheduleWithPlayer, shutUp,
    defaultLatency, timeStamp,

    -- for Live module
    installSound, playAtom,
    performanceTrans, messagesGrouped,

    -- for suppression of "unused" warnings :-)
    messagesGroupedManual, messagesSingly, messagesSimple,
  ) where

import qualified Haskore.Interface.SuperCollider.Schedule as Schedule
import Haskore.Interface.SuperCollider.Schedule (Time)

import qualified Haskore.Interface.SuperCollider.Performance   as SCPf
import qualified Haskore.Interface.SuperCollider.SoundMap      as SoundMap

import Haskore.Interface.SuperCollider.SoundMap (Instrument, Sound)

-- import qualified Sound.SC3.UGen.UGen as SCUGen

import qualified Sound.SC3.Server.PlayEasy  as SCPlay

-- import Sound.SC3.UGen.UGen (UGen)

import Sound.OSC.Transport.Monad (Transport, waitReply, )
import qualified Sound.OSC.Type as OSC
import qualified Sound.OSC.Time as OSCTime


import qualified Haskore.Melody            as Melody
-- import qualified Haskore.Music.Rhythmic    as RhyMusic
import qualified Haskore.Music             as Music

import qualified Haskore.Interface.SuperCollider.Timer as SCTimer
import qualified Haskore.RealTime.Timer                as Timer
import qualified Haskore.RealTime.EventList.TimeBody   as TimeList

import qualified Numeric.NonNegative.Wrapper as NonNeg

import Control.Monad.IO.Class (MonadIO, )
import Control.Monad (liftM, )



{- * Play several representations of music -}

music :: Ord note =>
   Time ->
   Schedule.SoundHandler note ->
   Music.T note ->
   IO ()
music latency soundHandler =
   schedule latency .
   Schedule.fromMusic soundHandler

{-
-- slightly inconsistent naming with respect to Music.fromMelodyNullAttr
rhythmicMusic :: (Show instr, Show drum, Ord instr, Ord drum) =>
   Double ->
   SoundMap.DrumTable drum ->
   SoundMap.InstrumentTable instr ->
   RhyMusic.T drum instr -> IO ()
rhythmicMusic latency drumMap instrMap =
   schedule latency .
   Schedule.fromRhythmicMusic drumMap instrMap

rhythmicMusicWithAttributes :: (Ord instr, Ord drum) =>
   Double ->
   SoundMap.DrumTableWithAttributes drum ->
   SoundMap.InstrumentTableWithAttributes instr ->
   RhyMusic.T drum instr -> IO ()
rhythmicMusicWithAttributes latency drumMap instrMap =
   schedule latency .
   Schedule.fromRhythmicMusicWithAttributes drumMap instrMap
-}

melody :: Time -> Instrument -> Melody.T () -> IO ()
melody latency sound =
   schedule latency .
   Schedule.fromMelody sound


performanceTrans :: (Transport m, MonadIO m) =>
   Time ->
   [OSC.Message] ->
   SCPf.T Time ->
   m ()
performanceTrans latency installMsgs =
   scheduleWithPlayer
      (messagesGrouped SCTimer.timer latency) .
   Schedule.fromPerformance installMsgs []

performance ::
   Time ->
   [OSC.Message] ->
   SCPf.T Time ->
   IO ()
performance latency installMsgs =
   schedule latency .
   Schedule.fromPerformance installMsgs []


schedule ::
   Time ->
   Schedule.T ->
   IO ()
schedule latency =
   SCPlay.withSC3 .
   scheduleWithPlayer
      (messagesGrouped SCTimer.timer latency)


shutUp :: IO ()
shutUp =
   do SCPlay.withSC3 SCPlay.stop
      return ()


{- * Initialization of replay -}

{- |
Default value for latency used in "Haskore.Interface.SuperCollider.Play.Live"
and connected modules.
-}
defaultLatency :: Time
defaultLatency = 0.1

-- should be moved to Hsc or supercollider-ht
timeStamp :: Time -> Time -> [OSC.Message] -> OSC.Bundle
timeStamp start t msgs = OSC.Bundle (OSCTime.ut_to_ntpr $ NonNeg.toNumber (start+t)) msgs


installSound ::
   (Transport m, SoundMap.SoundParameters params) =>
   String ->
   Sound params ->
   m ()
installSound name instr =
   SCPlay.simpleSync $ Schedule.installSoundMsg name Schedule.defaultChannel instr


playAtom :: Transport m =>
      SCPlay.NodeId
   -> String
   -> [(String,Double)]
   -> m ()
playAtom sid name params =
   SCPlay.send (Schedule.atomPlayMsg sid name params)



{- * Different approaches to timing -}


{-
How to synchronize to different messages simultaneously?

this behavior is application specific. SuperCollider sends '/done'
replies for each command tagged 'asynchronous', i.e. messages that
are deferred to the non-realtime-thread. so yes, if you want to make
sure each command completes before sending the next one, you need one
wait per asynchronous command.

the '/sync' message is also very useful in asynchronous
communication: it implements a barrier identified by an integer id,
so you can chunk asynchronous commands that don't have a mutual
dependency. the sf site seems to be down, here's the excerpt from
Server-Command-Reference.html:

"/sync                                  notify when async commands have completed.
        int - a unique number identifying this command.

Replies with a /synced message when all asynchronous commands
received before this one have completed. The reply will contain the
sent unique ID.
Asynchronous. Replies to sender with /synced, ID when complete."

to avoid a race, i think you need to send all the commands and /sync
in the same bundle, or else /sync might arrive before the last
command and consequently you might get /synced too early. that's not
a problem with TCP, of course.

-}

scheduleWithPlayer :: Transport m =>
   (TimeList.T Time OSC.Message -> m ()) ->
   Schedule.T ->
   m ()
scheduleWithPlayer player sc =
   do {- both variants do only work,
         if only synchronous commands are contained -}
      {-
      mapM_
         Trans.simpleSync
         (Schedule.initial sc)
      -}
      {-
      Synchronization of messages:
      http://lists.lurk.org/pipermail/haskell-art/2008-February/000102.html
      -}
      let isc = Schedule.initial sc
      mapM_ SCPlay.send isc
      mapM_ (const (waitReply "/done" >> return ())) isc

      player (Schedule.body sc)
      return ()

{- |
Uses @threadDelay@ and @getClockTime@ for a long-time stable,
but not very accurate timing.
Use time stamped messages for SuperCollider in order
to achieve exact scheduling.
You have to specify the latency,
that is, the maximal expected delay of creating and sending messages
to the SuperCollider server.
-}
messagesGrouped :: (Transport m) =>
   Timer.T m ->
   Time ->
   TimeList.T Time OSC.Message -> m ()
messagesGrouped timer latency =
   liftM (const ()) .
   TimeList.runTimeStampGrouped timer
      (\time -> SCPlay.send . timeStamp latency time)
{-
      (\time msgs ->
         liftIO (print time) >>
         SCPlay.send (timeStamp latency time msgs))
-}

{-
Group events before quantization.
This results in splitted events which actually should occur at the same time.
This makes SuperCollider switching the order messages in certain occasions.
-}
messagesGroupedManual :: (Transport m) =>
   Timer.T m ->
   Time ->
   TimeList.T Time OSC.Message -> m ()
messagesGroupedManual timer latency =
   liftM (const ()) .
   -- (\ pf -> liftIO Timer.getTimeSeconds >>= \startTime ->
   TimeList.runTimeStamp timer
      (\time ->
         {-
         putStr "current time: " >>
         Timer.getTimeSeconds >>= print >>
         putStrLn ("schedule time: " ++ show(time+latency)) >>
         -}
         {-
         (liftIO $ putStr (show (time-startTime) ++ ": ") >>
                   (print $ map eventToMark es)) >>
         -}
         (SCPlay.send . timeStamp latency time)) -- pf)
     . TimeList.collectCoincident

messagesSingly :: (Transport m) =>
   Timer.T m ->
   Time ->
   TimeList.T Time OSC.Message -> m ()
messagesSingly timer latency =
   liftM (const ()) .
   TimeList.runTimeStamp timer
      (\time ->
         -- (liftIO $ print $ eventToMark e) >>
         (SCPlay.send . timeStamp latency time . (:[])))


messagesSimple :: (Transport m) =>
   Timer.T m ->
   TimeList.T Time OSC.Message ->
   m ()
messagesSimple timer =
   liftM (const ()) .
   TimeList.run timer SCPlay.send
