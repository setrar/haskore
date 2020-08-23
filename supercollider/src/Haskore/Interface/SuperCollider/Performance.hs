module Haskore.Interface.SuperCollider.Performance where

import qualified Haskore.Interface.SuperCollider.Note     as Note
import qualified Haskore.Interface.SuperCollider.SoundMap as InstrMap

import qualified Haskore.Music.Standard as StdMusic
import qualified Haskore.Music.Rhythmic as RhyMusic
import qualified Haskore.Music          as Music
import qualified Haskore.Melody         as Melody

import qualified Haskore.Performance         as Pf
import qualified Haskore.Performance.BackEnd as PfBE
import qualified Haskore.Performance.Fancy   as FancyPf

import qualified Haskore.RealTime.EventList.TimeBody    as TimeList
import qualified Haskore.RealTime.EventList.TimeTime as TimeListPad
import qualified Haskore.General.IdGenerator      as IdGen

import qualified Numeric.NonNegative.Class as NonNeg



fromMelody :: (NonNeg.C time, Floating time, RealFrac time) =>
   Melody.T () -> PfBE.Padded time Note.T
fromMelody =
{-
   PfBE.fromPaddedPerformance
      (Note.fromRhythmicNote
          (const "no drum")
          (const "no instrument")) .
-}
   PfBE.fromPaddedPerformance
      (Note.fromRhythmicNote
          (error "no drum defined")
          (error "no instrument defined")) .
   fancyPaddedPerformanceFromMusic .
   StdMusic.fromMelodyNullAttr undefined


fancyPaddedPerformanceFromMusic ::
   (Ord note, NonNeg.C time, RealFrac time) =>
   Music.T note -> Pf.Padded time Double note
fancyPaddedPerformanceFromMusic =
   FancyPf.paddedFromMusic



type NodeId = Int   -- cf. PlayEasy
type NodeIdGen a = IdGen.T NodeId a
type T time = TimeListPad.T time (NodeId, Maybe (time, Note.T))


eventsFromNotes, eventsFromNotesQueue, eventsFromNotesEither ::
   (NonNeg.C time)
   => PfBE.Padded time Note.T
   -> NodeIdGen (T time)
eventsFromNotes = eventsFromNotesEither



eventsFromNotesQueue =
   eventsFromNotesQueueAux TimeList.empty

{- |
This variant does not need a list of 'Either's,
instead it uses two separate lists for start and stop events.
We would like to use a standard datatype for priority queues,
however it must be efficient to remove the first element
which means that the time stamp of all following elements must be decreased.
Since we have no such data structure, we simply use a TimeList.
But then again, without the 'Either' list,
we have to merge the queue of stop events and the list of start events manually.
This makes the implementation less beautiful.
-}
eventsFromNotesQueueAux :: (NonNeg.C time)
   => TimeList.T time NodeId
   -> PfBE.Padded time Note.T
   -> NodeIdGen (T time)
eventsFromNotesQueueAux queue xtt =
   let (qExists, (~(qTime,oldSId), qs)) =
          maybe
             (False, (error "no q", error "no qs"))
             (\q' -> (True, q'))
             (TimeList.viewL queue)
       (xTime,xs0) = TimeListPad.viewTimeL xtt
       (xExists, (note, xs)) =
          maybe
             (False, (error "no x", error "no xs"))
             (\x' -> (True, x'))
             (TimeListPad.viewBodyL xs0)
       qFirst =
          do IdGen.free oldSId
             fmap
                (TimeListPad.cons qTime (oldSId, Nothing))
                (eventsFromNotesQueueAux qs
                   (TimeListPad.decreaseStart qTime xtt))
       xFirst =
          do newSId <- IdGen.alloc
             let noteDur = PfBE.eventDur note
             fmap
                (TimeListPad.cons xTime
                   (newSId, Just (noteDur, PfBE.eventNote note)))
                (eventsFromNotesQueueAux
                   (TimeList.insert noteDur newSId
                      (TimeList.decreaseStart xTime queue))
                   xs)
   in  if qExists && (not xExists || qTime <= xTime)
         then qFirst
         else
           if xExists
             then xFirst
             else return (TimeListPad.pause xTime)


eventsFromNotesEither =
   eventsFromNotesEitherAux . TimeListPad.mapBody Right

{-
It seems not to be possible to implement that with foldM,
because one recursive call to eventsFromNotes
requires modifying the events list.
-}
eventsFromNotesEitherAux :: (NonNeg.C time)
   => TimeListPad.T time (Either NodeId (PfBE.Event time Note.T))
   -> NodeIdGen (T time)
eventsFromNotesEitherAux =
   (\ (dur, xss) ->
       fmap (TimeListPad.consTime dur) $
       maybe
          (return TimeListPad.empty)
          (\(x,xs) ->
             let doRest ev restAct =
                    fmap (TimeListPad.consBody ev) restAct
             in  either
                   (\sid ->
                       do IdGen.free sid
                          doRest
                             (sid, Nothing)
                             (eventsFromNotesEitherAux xs))
                   (\note ->
                       do sid <- IdGen.alloc
                          let noteDur = PfBE.eventDur note
                          doRest
                             (sid, Just (noteDur, PfBE.eventNote note))
                             (eventsFromNotesEitherAux
                                (TimeListPad.insert noteDur (Left sid) xs)))
                   x) $
       TimeListPad.viewBodyL xss)
    . TimeListPad.viewTimeL


instrStartNodeId :: NodeId
instrStartNodeId = 2 -- succ Sound.SC3.Server.Play.homeId

fixNodeIds :: NodeIdGen a -> a
fixNodeIds = IdGen.run instrStartNodeId

fromMusic :: (Ord note, NonNeg.C time, RealFrac time) =>
   Note.FromNote time note ->
   Music.T note ->
   NodeIdGen (T time)
fromMusic makeNote =
   eventsFromNotes .
   PfBE.fromPaddedPerformance makeNote .
   FancyPf.paddedFromMusic

{- needed for Play.Install, Play.Channel, Live -}
{-
fromRhythmicMusic ::
   (Show drum, Show instr, Ord drum, Ord instr,
    Floating time, RealFrac time) =>
   RhyMusic.T drum instr ->
   NodeIdGen (T time)
fromRhythmicMusic =
   fromMusic (Note.fromRhythmicNote show show)
-}

fromRhythmicMusicWithAttributes ::
   (Ord drum, Ord instr,
    NonNeg.C time, Floating time, RealFrac time) =>
   InstrMap.ToSound drum ->
   InstrMap.ToSound instr ->
   RhyMusic.T drum instr ->
   NodeIdGen (T time)
fromRhythmicMusicWithAttributes dMap iMap =
   fromMusic (Note.fromRhythmicNoteWithAttributes dMap iMap)
