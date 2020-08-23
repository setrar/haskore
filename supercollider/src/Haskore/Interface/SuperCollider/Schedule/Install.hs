{- |
This is a variant of the "Haskore.Interface.SuperCollider.Schedule" module.
Instead of an abstract @Instrument@ type
and a "Haskore.Interface.SuperCollider.SoundMap",
we attach SuperCollider instrument information to each note.
-}
module Haskore.Interface.SuperCollider.Schedule.Install where

import Sound.SC3.UGen.Type (UGen)

import qualified Sound.OSC.Type as OSC

import qualified Haskore.Interface.SuperCollider.Example       as Example
import qualified Haskore.Interface.SuperCollider.Play          as Play
import qualified Haskore.Interface.SuperCollider.Schedule      as Schedule
import qualified Haskore.Interface.SuperCollider.Note          as Note
import qualified Haskore.Interface.SuperCollider.SoundMap      as SoundMap

import Haskore.Interface.SuperCollider.SoundMap
          (DrumParameters, InstrumentParameters, AttributeList)

import qualified Haskore.Composition.Drum   as Drum
import qualified Haskore.Composition.Rhythm as Rhythm

import qualified Haskore.Music.Rhythmic as RhyMusic
import           Haskore.Music.Rhythmic (qn)
import qualified Haskore.Music          as Music
import           Haskore.Melody         as Melody


import Control.Monad.Trans.Writer (Writer, tell, runWriter)
import Control.Monad (liftM, ap, )
import Control.Applicative (Applicative, pure, (<*>), )



{- |
The @[OSC.Message]@ state is used for messages for installing the instruments.
We cannot use a Writer monad for this purpose
because we have to read the generated messages for 'playMusic'.
-}
newtype Environment a =
   Environment {unwrapEnvironment :: Writer [OSC.Message] a}

instance Functor Environment where
   fmap = liftM

instance Applicative Environment where
   (<*>) = ap
   pure = return

instance Monad Environment where
   x >>= y  = Environment $ unwrapEnvironment . y =<< unwrapEnvironment x
   return   = Environment . return

writeOSC :: [OSC.Message] -> Environment ()
writeOSC = Environment . tell


data Sound params attr =
       Sound
          {nameFromSound        :: String,
           flattenAttrFromSound :: (attr -> AttributeList)}

type Instrument attr = Sound InstrumentParameters attr
type Drum       attr = Sound DrumParameters attr



installSound ::
   SoundMap.SoundParameters params =>
   (parameterTuple -> AttributeList, graph -> SoundMap.Sound params) ->
   String ->
   graph ->
   Environment (Sound params parameterTuple)
installSound (makeAttributeList, makeSoundUGen) name instr =
   writeOSC [Schedule.installSoundMsg name
               Schedule.defaultChannel (makeSoundUGen instr)] >>
   return (Sound name makeAttributeList)


installSound0 ::
   SoundMap.SoundParameters params =>
   String ->
   SoundMap.Sound params ->
   Environment (Sound params ())
installSound0 =
   installSound SoundMap.with0Attributes

installSound1 ::
   SoundMap.SoundParameters params =>
   String ->
   (UGen -> SoundMap.Sound params) ->
   Environment (Sound params Double)
installSound1 =
   installSound SoundMap.with1Attribute

installSound2 ::
   SoundMap.SoundParameters params =>
   String ->
   (UGen -> UGen -> SoundMap.Sound params) ->
   Environment (Sound params (Double,Double))
installSound2 =
   installSound SoundMap.with2Attributes



-- cf. SoundMap.ToSound
data SoundAttributes params = SoundAttributes AttributeList String
   deriving (Eq, Ord)

type DrumAttributes       = SoundAttributes DrumParameters
type InstrumentAttributes = SoundAttributes InstrumentParameters


rhythmicMusicFromDynamicMelody ::
   Instrument attr ->
   Melody.T (Rational, attr) ->
   RhyMusic.T DrumAttributes InstrumentAttributes
rhythmicMusicFromDynamicMelody (Sound name flattenAttr) =
   RhyMusic.fromMelody
      (\(vel,attr) -> (vel, SoundAttributes (flattenAttr attr) name))

rhythmicMusicFromMelody ::
   Instrument attr ->
   Melody.T attr ->
   RhyMusic.T DrumAttributes InstrumentAttributes
rhythmicMusicFromMelody instr =
   rhythmicMusicFromDynamicMelody instr .
   Music.mapNote (\(Melody.Note attr p) -> Melody.Note (1,attr) p)



rhythmicMusicFromRhythm ::
   Drum () ->
   Music.Dur ->
   String ->
   RhyMusic.T DrumAttributes InstrumentAttributes
rhythmicMusicFromRhythm sound dur =
   Rhythm.toMusicWithDrumUnit dur (SoundAttributes [] (nameFromSound sound)) .
   Rhythm.fromString

rhythmicMusicFromDrum ::
   Drum () ->
   Music.Dur ->
   RhyMusic.T DrumAttributes InstrumentAttributes
rhythmicMusicFromDrum sound dur =
   Drum.toMusicDefaultAttr (SoundAttributes [] (nameFromSound sound)) dur


fromRhythmicMusic ::
   Environment (RhyMusic.T DrumAttributes InstrumentAttributes) ->
   Schedule.T
fromRhythmicMusic genMusic =
   let (music, installInstruments) = runWriter $ unwrapEnvironment genMusic
   in  Schedule.fromMusicMsgs
          (Note.fromRhythmicNoteWithAttributes
              (\(SoundAttributes params name) -> (params,name))
              (\(SoundAttributes params name) -> (params,name)),
           installInstruments) $
       music


example :: IO ()
example =
   Play.schedule Play.defaultLatency $ fromRhythmicMusic $
      do sawPerc <- installSound0 "saw percussion" Example.sawPerc
         dynPerc <- installSound1 "detuned bass"   Example.dynPerc
         let mel = rhythmicMusicFromMelody sawPerc $ Music.transpose 12 $ Music.line
               [c 0 qn (), e 0 qn (),
                Music.chord [c 0 qn (), e 0 qn (), g 0 qn ()]]
             bass = rhythmicMusicFromMelody dynPerc $ Music.line
               [c 0 qn 0.001, c 0 qn 0.003, c 0 qn 0.01]
         return $
            Music.changeTempo 2 $
            Music.chord [mel,bass]
