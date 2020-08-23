{- |
This module is quite specific to "Haskore.Music.Rhythmic".
Maybe the module name should reflect this?
-}
module Haskore.Interface.SuperCollider.SoundMap where

import Haskore.Interface.SuperCollider.Channel (Channel)
import qualified Haskore.Interface.SuperCollider.Channel as Channel

import qualified Sound.SC3.Server.PlayEasy as SCPlay
import qualified Sound.SC3.UGen.Rate as SCRate
import qualified Sound.SC3.UGen.UGen as SCUGen

import Sound.SC3.UGen.Type (UGen)

import Data.Maybe.HT (toMaybe, )
import Data.Maybe (mapMaybe, )


{- * Generic definitions -}

type Name = String

{- |
'Attribute' means an optional information for a note.
Compare with 'InstrumentParameters'.
-}
type Attribute = Double
type AttributeList = [Attribute]
type ToSound instr = instr -> (AttributeList, Name)


attributeControl :: Int -> UGen
attributeControl n =
   SCUGen.control SCRate.KR (attributeName n) 0

attributeName :: Int -> String
attributeName n = "attribute" ++ show n

attributeNames :: [String]
attributeNames = map attributeName [0..]

control :: String -> UGen
control name = SCUGen.control SCRate.KR name 0

pitchName :: String
pitchName = "pitch"

velocityName :: String
velocityName = "velocity"

durationName :: String
durationName = "duration"





{- * Generic sound maps -}

with0Attributes ::
   (() -> AttributeList,
       (sound) -> sound)
with0Attributes =
   (\() -> [],
    \sound -> sound)

with1Attribute ::
   ((Attribute) -> AttributeList,
       (UGen -> sound) -> sound)
with1Attribute =
   (\(p0) -> [p0],
    \sound -> sound
      (attributeControl 0))

with2Attributes ::
   ((Attribute, Attribute) -> AttributeList,
       (UGen -> UGen -> sound) -> sound)
with2Attributes =
   (\(p0,p1) -> [p0,p1],
    \sound -> sound
      (attributeControl 0)
      (attributeControl 1))

with3Attributes ::
   ((Attribute, Attribute, Attribute) -> AttributeList,
       (UGen -> UGen -> UGen -> sound) -> sound)
with3Attributes =
   (\(p0,p1,p2) -> [p0,p1,p2],
    \sound -> sound
      (attributeControl 0)
      (attributeControl 1)
      (attributeControl 2))

with4Attributes ::
   ((Attribute, Attribute, Attribute, Attribute) -> AttributeList,
       (UGen -> UGen -> UGen -> UGen -> sound) -> sound)
with4Attributes =
   (\(p0,p1,p2,p3) -> [p0,p1,p2,p3],
    \sound -> sound
      (attributeControl 0)
      (attributeControl 1)
      (attributeControl 2)
      (attributeControl 3))




type Table params sound = [(sound, Sound params)]

type TableWithAttributes params sound = [Assign params sound]

type TableWithChannels params sound = [(Channel, Assign params sound)]

type Sound params = params -> UGen

class SoundParameters params where
    soundParameters :: params

ugenFromSound :: SoundParameters params => Sound params -> UGen
ugenFromSound sound = sound soundParameters

data Assign params sound =
        Assign Name (sound -> Maybe AttributeList) (Sound params)

lookup :: TableWithAttributes params sound -> ToSound sound
lookup table sound =
   case mapMaybe (\(Assign name toAttributes _) ->
                     fmap (\ps -> (ps,name)) (toAttributes sound)) table of
      [x] -> x
      []  -> error "SuperCollider.SoundMap.lookup: sound not found"
      _   -> error "SuperCollider.SoundMap.lookup: multiple sounds found"


assignGeneric ::
   (attributeTuple -> AttributeList, soundGen -> Sound params) ->
      Name ->
      (sound -> Maybe attributeTuple) ->
      soundGen ->
      Assign params sound
assignGeneric (makeAttributeList, makeSound) name select soundGen =
   Assign
      name
      (fmap makeAttributeList . select)
      (makeSound soundGen)


assign ::
   Name -> (sound -> Maybe ()) -> Sound params ->
      Assign params sound
assign = assignGeneric with0Attributes

-- simplified variant of 'assign' for comparable @sound@ types
assignEq :: Eq sound =>
   Name -> sound -> Sound params ->
      Assign params sound
assignEq name soundId =
   assign name (\x -> toMaybe (soundId==x) ())

assign1 ::
   Name -> (sound -> Maybe Attribute) ->
      (UGen -> Sound params) ->
      Assign params sound
assign1 = assignGeneric with1Attribute

assign2 ::
   Name -> (sound -> Maybe (Attribute, Attribute)) ->
      (UGen -> UGen -> Sound params) ->
      Assign params sound
assign2 = assignGeneric with2Attributes

assign3 ::
   Name -> (sound -> Maybe (Attribute, Attribute, Attribute)) ->
      (UGen -> UGen -> UGen -> Sound params) ->
      Assign params sound
assign3 = assignGeneric with3Attributes

assign4 ::
   Name -> (sound -> Maybe (Attribute, Attribute, Attribute, Attribute)) ->
      (UGen -> UGen -> UGen -> UGen -> Sound params) ->
      Assign params sound
assign4 = assignGeneric with4Attributes


withDuration :: (UGen -> Sound params) -> Sound params
withDuration ugen =
   ugen (control durationName)





{- * Instrument maps -}

{- |
@Parameter@ means an obligatory information for a note,
like @frequency@, @velocity@, @duration@.
Compare with 'Attribute'.
-}
data InstrumentParameters = InstrumentParameters {
        instrumentDuration  :: UGen,
        instrumentVelocity  :: UGen,
        instrumentFrequency :: UGen
      }

instrumentParameters :: InstrumentParameters
instrumentParameters =
   InstrumentParameters (control durationName) (control velocityName) (control pitchName)

instance SoundParameters InstrumentParameters where
   soundParameters = instrumentParameters


type InstrumentTable instr = Table InstrumentParameters instr

type InstrumentTableWithAttributes instr =
        TableWithAttributes InstrumentParameters instr

type InstrumentTableWithChannels instr =
        TableWithChannels InstrumentParameters instr

type Instrument = Sound InstrumentParameters

type InstrumentAssign instr =
        Assign InstrumentParameters instr

instrumentFromUGen :: (UGen -> UGen -> UGen) -> Instrument
instrumentFromUGen ugen params =
   ugen (instrumentVelocity params) (instrumentFrequency params)



{- * Drum maps -}

data DrumParameters = DrumParameters {
        drumDuration  :: UGen,
        drumVelocity  :: UGen
      }

drumParameters :: DrumParameters
drumParameters =
   DrumParameters (control durationName) (control velocityName)

instance SoundParameters DrumParameters where
   soundParameters = drumParameters


type DrumTable instr = Table DrumParameters instr

type DrumTableWithAttributes instr =
        TableWithAttributes DrumParameters instr

type DrumTableWithChannels instr =
        TableWithChannels DrumParameters instr

type Drum = Sound DrumParameters

type DrumAssign instr =
        Assign DrumParameters instr

drumFromUGen :: (UGen -> UGen) -> Drum
drumFromUGen ugen params =
   ugen (drumVelocity params)




{- * Sound Maps with Channel management -}

{- |
Like a State+Writer monad with the binding operation
baked into 'registerInstrument'.
This way we can suppress ignoring of results of 'registerInstrument',
which is easily possible with 'do' notation.
-}
newtype ChannelMap drum instr =
   ChannelMap {runChannelMap :: Channel ->
        (UGen, (DrumTableWithChannels drum, InstrumentTableWithChannels instr))}

registerSound ::
   params ->
   ((Channel, Assign params sound) ->
    (DrumTableWithChannels drum, InstrumentTableWithChannels instr) ->
    (DrumTableWithChannels drum, InstrumentTableWithChannels instr)) ->
   Assign params sound ->
   (UGen -> ChannelMap drum instr) ->
   ChannelMap drum instr
registerSound params insert ass@(Assign _ _ sound) nextCM =
   ChannelMap $ \chan ->
      let numChan      = SCPlay.mceDegree (sound params)
          ChannelMap f = nextCM (Channel.readUGen numChan chan)
          (globalUGen, tables) = f (chan + numChan)
      in  (globalUGen, insert (chan,ass) tables)

registerInstrument ::
   InstrumentAssign instr ->
   (UGen -> ChannelMap drum instr) ->
   ChannelMap drum instr
registerInstrument =
   registerSound instrumentParameters $
      \ (chan,ass) (drumTable, instrTable) ->
          (drumTable, (chan,ass) : instrTable)

registerDrum ::
   DrumAssign drum ->
   (UGen -> ChannelMap drum instr) ->
   ChannelMap drum instr
registerDrum =
   registerSound drumParameters $
      \ (chan,ass) (drumTable, instrTable) ->
          ((chan,ass) : drumTable, instrTable)

soundEffect ::
   UGen ->
   ChannelMap drum instr
soundEffect globalUGen =
   ChannelMap $ const (globalUGen, ([], []))
