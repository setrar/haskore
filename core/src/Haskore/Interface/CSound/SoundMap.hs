-- cf. SuperCollider.SoundMap
-- this module shall replace InstrumentMap in the long term
module Haskore.Interface.CSound.SoundMap where

import qualified Haskore.Interface.CSound.Orchestra as Orchestra

import Haskore.Interface.CSound.Orchestra
          (SigExp, noteDur, noteVel, notePit, pField, )
import Haskore.Interface.CSound (PField, Instrument, )

import Data.Maybe.HT (toMaybe, )
import Data.Maybe (mapMaybe, )


type SoundId = Instrument
type InstrumentId = SoundId
type DrumId = SoundId

type Attribute = PField
type AttributeList = [Attribute]
type ToSound instr = instr -> (AttributeList, SoundId)


attributeControl :: Int -> SigExp
attributeControl n = pField (6+n)


type InstrumentTable out instr = [(instr, InstrumentSigExp out)]

type InstrumentTableWithAttributes out instr = [InstrumentAssociation out instr]

type InstrumentSigExp out = SigExp -> SigExp -> SigExp -> out

data InstrumentAssociation out instr =
        InstrumentAssociation InstrumentId (instr -> Maybe AttributeList) out

lookupInstrument :: InstrumentTableWithAttributes out instr -> ToSound instr
lookupInstrument table instr =
   case mapMaybe (\(InstrumentAssociation name toAttributes _) ->
                     fmap (\ps -> (ps,name)) (toAttributes instr)) table of
      [] -> error "SuperCollider.InstrumentMap.lookup: instrument not found"
      [x] -> x
      _ -> error "SuperCollider.InstrumentMap.lookup: multiple instruments found"

instrumentTableToInstrBlocks ::
   InstrumentTableWithAttributes out instr -> [Orchestra.InstrBlock out]
instrumentTableToInstrBlocks =
   map (\(InstrumentAssociation i _ out) -> Orchestra.InstrBlock i 0 out [])

addInstrumentControls :: InstrumentSigExp out -> out
addInstrumentControls graph = graph noteDur noteVel notePit


instrumentAssociation ::
   (parameterTuple -> AttributeList) ->
   (graph -> InstrumentSigExp out) ->
      InstrumentId -> (instr -> Maybe parameterTuple) ->
      graph ->
      InstrumentAssociation out instr
instrumentAssociation makeAttributeList makeInstrumentSigExp name select graph =
   InstrumentAssociation
      name
      (fmap makeAttributeList . select)
      (addInstrumentControls $ makeInstrumentSigExp graph)


instrument ::
   InstrumentId -> (instr -> Maybe ()) -> (InstrumentSigExp out) ->
      InstrumentAssociation out instr
instrument = instrumentAssociation (\() -> []) id

-- simplified variant of 'instrument' for comparable @instrument@ types
instrumentEq :: Eq instrument =>
   InstrumentId -> instrument -> (InstrumentSigExp out) ->
      InstrumentAssociation out instrument
instrumentEq name instrumentId =
   instrument name (\x -> toMaybe (instrumentId==x) ())

instrument1 ::
   InstrumentId -> (instr -> Maybe Attribute) ->
      (SigExp -> InstrumentSigExp out) ->
      InstrumentAssociation out instr
instrument1 =
   instrumentAssociation
      (\p0 -> [p0])
      (\graph -> graph (attributeControl 0))


instrument2 ::
   InstrumentId -> (instr -> Maybe (Attribute, Attribute)) ->
      (SigExp -> SigExp -> InstrumentSigExp out) ->
      InstrumentAssociation out instr
instrument2 =
   instrumentAssociation
      (\(p0,p1) -> [p0,p1])
      (\graph -> graph
         (attributeControl 0)
         (attributeControl 1))

instrument3 ::
   InstrumentId -> (instr -> Maybe (Attribute, Attribute, Attribute)) ->
      (SigExp -> SigExp -> SigExp -> InstrumentSigExp out) ->
      InstrumentAssociation out instr
instrument3 =
   instrumentAssociation
      (\(p0,p1,p2) -> [p0,p1,p2])
      (\graph -> graph
         (attributeControl 0)
         (attributeControl 1)
         (attributeControl 2))

instrument4 ::
   InstrumentId -> (instr -> Maybe (Attribute, Attribute, Attribute, Attribute)) ->
      (SigExp -> SigExp -> SigExp -> SigExp -> InstrumentSigExp out) ->
      InstrumentAssociation out instr
instrument4 =
   instrumentAssociation
      (\(p0,p1,p2,p3) -> [p0,p1,p2,p3])
      (\graph -> graph
         (attributeControl 0)
         (attributeControl 1)
         (attributeControl 2)
         (attributeControl 3))



type DrumTable out drum = [(drum, DrumSigExp out)]

type DrumTableWithAttributes out drum = [DrumAssociation out drum]

type DrumSigExp out = SigExp -> SigExp -> out

data DrumAssociation out drum =
        DrumAssociation DrumId (drum -> Maybe AttributeList) out

lookupDrum :: DrumTableWithAttributes out drum -> ToSound drum
lookupDrum table drumId =
   case mapMaybe (\(DrumAssociation name toAttributes _) ->
                     fmap (\ps -> (ps,name)) (toAttributes drumId)) table of
      [] -> error "SuperCollider.InstrumentMap.lookup: drum not found"
      [x] -> x
      _ -> error "SuperCollider.InstrumentMap.lookup: multiple drums found"

drumTableToInstrBlocks :: DrumTableWithAttributes out instr -> [Orchestra.InstrBlock out]
drumTableToInstrBlocks =
   map (\(DrumAssociation i _ out) -> Orchestra.InstrBlock i 0 out [])

addDrumControls :: DrumSigExp out -> out
addDrumControls graph = graph noteDur noteVel

drumAssociation ::
   (parameterTuple -> AttributeList) ->
   (graph -> DrumSigExp out) ->
      DrumId -> (drum -> Maybe parameterTuple) ->
      graph ->
      DrumAssociation out drum
drumAssociation makeAttributeList makeDrumSigExp name select graph =
   DrumAssociation
      name
      (fmap makeAttributeList . select)
      (addDrumControls $ makeDrumSigExp graph)


drum ::
   DrumId -> (drum -> Maybe ()) -> (DrumSigExp out) ->
      DrumAssociation out drum
drum = drumAssociation (\() -> []) id

-- simplified variant of 'drum' for comparable @drum@ types
drumEq :: Eq drum =>
   DrumId -> drum -> (DrumSigExp out) ->
      DrumAssociation out drum
drumEq name drumId =
   drum name (\x -> toMaybe (drumId==x) ())

drum1 ::
   DrumId -> (drum -> Maybe Attribute) ->
      (SigExp -> DrumSigExp out) ->
      DrumAssociation out drum
drum1 =
   drumAssociation
      (\p0 -> [p0])
      (\graph -> graph (attributeControl 0))

drum2 ::
   DrumId -> (drum -> Maybe (Attribute, Attribute)) ->
      (SigExp -> SigExp -> DrumSigExp out) ->
      DrumAssociation out drum
drum2 =
   drumAssociation
      (\(p0,p1) -> [p0,p1])
      (\graph -> graph
         (attributeControl 0)
         (attributeControl 1))

drum3 ::
   DrumId -> (drum -> Maybe (Attribute, Attribute, Attribute)) ->
      (SigExp -> SigExp -> SigExp -> DrumSigExp out) ->
      DrumAssociation out drum
drum3 =
   drumAssociation
      (\(p0,p1,p2) -> [p0,p1,p2])
      (\graph -> graph
         (attributeControl 0)
         (attributeControl 1)
         (attributeControl 2))

drum4 ::
   DrumId -> (drum -> Maybe (Attribute, Attribute, Attribute, Attribute)) ->
      (SigExp -> SigExp -> SigExp -> SigExp -> DrumSigExp out) ->
      DrumAssociation out drum
drum4 =
   drumAssociation
      (\(p0,p1,p2,p3) -> [p0,p1,p2,p3])
      (\graph -> graph
         (attributeControl 0)
         (attributeControl 1)
         (attributeControl 2)
         (attributeControl 3))

