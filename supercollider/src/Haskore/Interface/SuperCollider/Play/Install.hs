{- |
This is a variant of the "Haskore.Interface.SuperCollider.Play" module.
Instead of an abstract @Instrument@ type
and a "Haskore.Interface.SuperCollider.SoundMap",
we attach SuperCollider instrument information to each note.

This module can be used as basis for life performances.
-}
module Haskore.Interface.SuperCollider.Play.Install
   {-# DEPRECATED "use Haskore.Interface.SuperCollider.Schedule.Install instead" #-}
   where

import qualified Sound.SC3.Server.PlayEasy  as SCPlay

import Sound.SC3.UGen.Type (UGen)

import Sound.OSC.Transport.Monad (Transport, )

import Haskore.Interface.SuperCollider.Schedule.Install
   (Sound(..),
    SoundAttributes(..), InstrumentAttributes, DrumAttributes,
    {- rhythmicMusicFromDynamicMelody, -} rhythmicMusicFromMelody,
    {- rhythmicMusicFromRhythm, rhythmicMusicFromDrum, -} )

import qualified Haskore.Interface.SuperCollider.Example       as Example
import qualified Haskore.Interface.SuperCollider.Play          as Play
import qualified Haskore.Interface.SuperCollider.Performance   as SCPf
import qualified Haskore.Interface.SuperCollider.SoundMap      as SoundMap

import Haskore.Interface.SuperCollider.SoundMap (AttributeList)

import qualified Haskore.Music.Rhythmic as RhyMusic
import           Haskore.Music.Rhythmic (qn)
import qualified Haskore.Music          as Music
import           Haskore.Melody         as Melody

import Control.Monad.IO.Class (MonadIO, )


installSound ::
   (Transport m, SoundMap.SoundParameters params) =>
   (parameterTuple -> AttributeList, graph -> SoundMap.Sound params) ->
   String ->
   graph ->
   m (Sound params parameterTuple)
installSound (makeAttributeList, makeSoundUGen) name instr =
   Play.installSound name (makeSoundUGen instr) >>
   return (Sound name makeAttributeList)


installSound0 ::
   (Transport m, SoundMap.SoundParameters params) =>
   String ->
   SoundMap.Sound params ->
   m (Sound params ())
installSound0 =
   installSound SoundMap.with0Attributes

installSound1 ::
   (Transport m, SoundMap.SoundParameters params) =>
   String ->
   (UGen -> SoundMap.Sound params) ->
   m (Sound params Double)
installSound1 =
   installSound SoundMap.with1Attribute

installSound2 ::
   (Transport m, SoundMap.SoundParameters params) =>
   String ->
   (UGen -> UGen -> SoundMap.Sound params) ->
   m (Sound params (Double,Double))
installSound2 =
   installSound SoundMap.with2Attributes




playMusic :: (Transport m, MonadIO m) =>
   RhyMusic.T DrumAttributes InstrumentAttributes -> m ()
playMusic =
   Play.performanceTrans Play.defaultLatency [] .
   SCPf.fixNodeIds .
   SCPf.fromRhythmicMusicWithAttributes
      (\(SoundAttributes attrs name) -> (attrs,name))
      (\(SoundAttributes attrs name) -> (attrs,name))



example :: IO ()
example =
   SCPlay.withSC3 $
      do SCPlay.reset
         sawPerc <- installSound0 "saw percussion" Example.sawPerc
         dynPerc <- installSound1 "detuned bass"   Example.dynPerc
         let mel = rhythmicMusicFromMelody sawPerc $ Music.transpose 12 $ Music.line
               [c 0 qn (), e 0 qn (),
                Music.chord [c 0 qn (), e 0 qn (), g 0 qn ()]]
             bass = rhythmicMusicFromMelody dynPerc $ Music.line
               [c 0 qn 0.001, c 0 qn 0.003, c 0 qn 0.01]
         playMusic $
            Music.changeTempo 2 $
            Music.chord [mel,bass]

