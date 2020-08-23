module Haskore.Interface.SuperCollider.Schedule (
    T(..), SoundHandler, Time,

    fromMelody, fromMusic, fromMusicGlobalEffect,
    fromRhythmicMusicSoundEffects,
    fromPerformance,
    rhythmicMusic, rhythmicMusicWithAttributes,

    -- for Play module
    installUGenMsg, installSoundMsg,
    defaultChannel, atomPlayMsg,

    -- for suppression of "unused" warnings :-)
    fromMusicMsgs, eventToMark,

    -- testing
    -- fromMelodyPerformance,
  ) where

import qualified Sound.SC3.Server.Command   as SCCmd
import qualified Sound.SC3.Server.PlayEasy  as SCPlay
import Sound.SC3.Server.Enum (AddAction(AddToTail, AddToHead))

import Sound.SC3.UGen.Type (UGen)

import qualified Sound.OSC.Type as OSC

import qualified Haskore.Interface.SuperCollider.Note          as Note
import qualified Haskore.Interface.SuperCollider.Performance   as SCPf
import qualified Haskore.Interface.SuperCollider.SoundMap      as SoundMap

import Haskore.Interface.SuperCollider.SoundMap (Instrument, Sound)

import Haskore.Interface.SuperCollider.Channel (Channel, NumberChannels)
import qualified Haskore.Interface.SuperCollider.Channel as Channel

import qualified Haskore.Melody            as Melody
import qualified Haskore.Music.Rhythmic    as RhyMusic
import qualified Haskore.Music             as Music

import qualified Haskore.Performance.BackEnd      as PfBE
import qualified Haskore.RealTime.EventList.TimeBody as TimeList
import qualified Haskore.RealTime.EventList.TimeTime as TimeListPad

import qualified Haskore.General.IdGenerator      as IdGen

import qualified Numeric.NonNegative.Wrapper as NonNeg

import Control.Monad (liftM2, )

import Data.Maybe(fromMaybe, maybeToList, )


{- * Schedule data structure -}


data T =
   Cons {
      initial :: [OSC.Message]
          {- ^ All of these messages must be synchronous!
               Otherwise the player might wait forever. -},
      body    :: TimeList.T Time OSC.Message
   }
    deriving Show

type SoundAssign = (SoundMap.Name, UGen)

type SoundHandler note =
        (Note.FromNote NonNeg.Double note, [SoundAssign])

type Time = NonNeg.Double


fromMusic :: Ord note =>
   SoundHandler note ->
   Music.T note ->
   T
fromMusic (makeNote, soundAssigns) =
   fromPerformance (installSoundsMsg defaultChannel soundAssigns) [] .
   SCPf.fixNodeIds .
   SCPf.fromMusic makeNote

fromMusicMsgs :: Ord note =>
   (Note.FromNote NonNeg.Double note, [OSC.Message]) ->
   Music.T note ->
   T
fromMusicMsgs (makeNote, installSounds) =
   fromPerformance installSounds [] .
   SCPf.fixNodeIds .
   SCPf.fromMusic makeNote

fromMusicGlobalEffect :: Ord note =>
   SoundHandler note ->
   (UGen -> UGen, NumberChannels) ->
   Music.T note ->
   T
fromMusicGlobalEffect
      (makeNote, soundAssigns) (globalEffect, numChan) music =
   let effectsName = "global effects"
       instrumentChannel = defaultChannel+numChan
       effectChannel = defaultChannel
       (sid,pf) =
          SCPf.fixNodeIds $
          liftM2 (,)
             IdGen.alloc
             (SCPf.fromMusic makeNote music)
   in  fromPerformance
          (installUGenMsg effectsName effectChannel
              (globalEffect (Channel.readUGen numChan instrumentChannel)) :
           installSoundsMsg instrumentChannel soundAssigns)
          [SCCmd.s_new effectsName sid AddToTail SCPlay.homeId []]
          pf

fromRhythmicMusicSoundEffects ::
   (Ord drum, Ord instr) =>
   SoundMap.ChannelMap instr drum ->
   NumberChannels ->
   RhyMusic.T instr drum ->
   T
fromRhythmicMusicSoundEffects cMap numChan music =
   let effectsName = "global effects"
       instrumentChannel = defaultChannel+numChan
       effectChannel = defaultChannel
       (globalEffect, (drumCMap, instrCMap)) =
          SoundMap.runChannelMap cMap instrumentChannel
       (drumChannels, drumMap)  = unzip drumCMap
       (instrChannels,instrMap) = unzip instrCMap
       (makeNote, soundAssigns) =
          rhythmicMusicWithAttributes drumMap instrMap
       soundChannelAssigns =
          zipWith
             (\chan (name,ugen) -> (name,chan,ugen))
             (drumChannels++instrChannels) soundAssigns
       (sid,pf) =
          SCPf.fixNodeIds $
          liftM2 (,)
             IdGen.alloc
             (SCPf.fromMusic makeNote music)
   in  fromPerformance
          (installUGenMsg effectsName effectChannel globalEffect :
           installChannelSoundsMsg soundChannelAssigns)
          [SCCmd.s_new effectsName sid AddToTail SCPlay.homeId []]
          pf

-- slightly inconsistent naming with respect to Music.fromMelodyNullAttr
rhythmicMusic :: (Show instr, Show drum, Ord instr, Ord drum) =>
   SoundMap.DrumTable drum ->
   SoundMap.InstrumentTable instr ->
   SoundHandler (RhyMusic.Note drum instr)
rhythmicMusic drumMap instrMap =
   (Note.fromRhythmicNote show show,
    map extractSoundAssign drumMap ++
    map extractSoundAssign instrMap)

rhythmicMusicWithAttributes :: (Ord instr, Ord drum) =>
   SoundMap.DrumTableWithAttributes drum ->
   SoundMap.InstrumentTableWithAttributes instr ->
   SoundHandler (RhyMusic.Note drum instr)
rhythmicMusicWithAttributes drumMap instrMap =
   (Note.fromRhythmicNoteWithAttributes
       (SoundMap.lookup drumMap)
       (SoundMap.lookup instrMap),
    -- fromRhythmicMusicSoundEffects relies on that order
    map extractSoundWithAttributesAssign drumMap ++
    map extractSoundWithAttributesAssign instrMap)



fromMelody ::
   Instrument ->
   Melody.T () ->
   T
fromMelody sound =
   fromMelodyPerformance sound .
   SCPf.fromMelody


melodyRestError :: a
melodyRestError =
   error "SuperCollider.Play: Music from Melody contains a rest."


fromMelodyPerformance ::
   Instrument ->
   PfBE.Padded Time Note.T ->
   T
fromMelodyPerformance sound =
   let name = "mono instrument"

       sid = SCPf.instrStartNodeId
       installSoundEvent =
          installSoundMsg name defaultChannel sound
       playSoundEvent =
          atomPlayMsg sid name []

       events =
          flip TimeListPad.snocBody stopMsg .
          TimeListPad.mapBody
             (\note ->
                SCCmd.n_set sid
                   [(SoundMap.pitchName, NonNeg.toNumber $ fromMaybe melodyRestError $ Note.pitch note),
                    (SoundMap.velocityName, NonNeg.toNumber (Note.velocity note))]) .
          TimeListPad.mapBody PfBE.eventNote

   in  Cons (installSoundEvent : []) .
       prependInitialMsgs (resetMsgs ++ playSoundEvent : []) . events


fromPerformance ::
   [OSC.Message] ->
   [OSC.Message] ->
   SCPf.T Time ->
   T
fromPerformance installInstruments globalEffects pf =
   Cons
      installInstruments
      (prependInitialMsgs
         (resetMsgs ++ globalEffects) (messagesFromPerformance pf))


prependInitialMsgs :: [OSC.Message] -> TimeList.T Time OSC.Message -> TimeList.T Time OSC.Message
prependInitialMsgs = flip (foldr (TimeList.cons 0))

messagesFromPerformance :: SCPf.T Time -> TimeList.T Time OSC.Message
messagesFromPerformance =
   flip TimeListPad.snocBody stopMsg .
   TimeListPad.mapBody (uncurry eventToMsg)



{- * Construction of OpenSoundControl messages -}

stopMsg :: OSC.Message
stopMsg = SCCmd.g_freeAll [SCPlay.homeId]


resetMsgs :: [OSC.Message]
resetMsgs =
   [SCCmd.g_freeAll [SCPlay.rootId],
    SCCmd.g_new [(SCPlay.homeId, AddToTail, SCPlay.rootId)]]


atomPlayMsg ::
   SCPlay.NodeId ->
   String ->
   [(String, Double)] ->
   OSC.Message
atomPlayMsg sid name =
   SCCmd.s_new name sid AddToHead SCPlay.homeId
   -- adding to head is necessary for allowing global effects like in the Live module


installUGenMsg ::
   String ->
   Channel ->
   UGen ->
   OSC.Message
installUGenMsg name chan =
   SCPlay.d_recv_synthdef name .
   Channel.writeUGen chan

installSoundMsg ::
   SoundMap.SoundParameters params =>
   String ->
   Channel ->
   Sound params ->
   OSC.Message
installSoundMsg name chan =
   installUGenMsg name chan . SoundMap.ugenFromSound


defaultChannel :: Channel
defaultChannel = 0


installSoundsMsg ::
   Channel ->
   [SoundAssign] ->
   [OSC.Message]
installSoundsMsg channel =
   map (\(name,sound) ->
           installUGenMsg name channel sound)

installChannelSoundsMsg ::
   [(SoundMap.Name, Channel, UGen)] ->
   [OSC.Message]
installChannelSoundsMsg =
   map (\(name, channel, ugen) ->
           installUGenMsg name channel ugen)

extractSoundAssign ::
   (Show instr, SoundMap.SoundParameters params) =>
   (instr, Sound params) -> SoundAssign
extractSoundAssign =
   \(instr, sound) ->
    (show instr, SoundMap.ugenFromSound sound)

extractSoundWithAttributesAssign ::
   SoundMap.SoundParameters params =>
   SoundMap.Assign params instr -> SoundAssign
extractSoundWithAttributesAssign =
   \(SoundMap.Assign name _ sound) -> (name, SoundMap.ugenFromSound sound)


eventToMark ::
   (SCPlay.NodeId, Maybe Note.T) ->
   (SCPlay.NodeId, Bool)
eventToMark (sid, note) =
   (sid, maybe False (const True) note)

eventToMsg :: SCPlay.NodeId -> Maybe (Time, Note.T) -> OSC.Message
eventToMsg sid =
   maybe
      (SCCmd.n_free [sid])
      (\(dur,note) ->
         atomPlayMsg sid
            (Note.instrument note)
            (map ((,) SoundMap.pitchName . NonNeg.toNumber)
                 (maybeToList (Note.pitch note)) ++
             (SoundMap.velocityName, NonNeg.toNumber (Note.velocity note)) :
             (SoundMap.durationName, NonNeg.toNumber dur) :
             zip SoundMap.attributeNames (Note.parameters note)))

