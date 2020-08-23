{-# LANGUAGE NoImplicitPrelude #-}
module Haskore.Interface.Signal.Write where

{-
What does the MIDI volume mean?
First one must distinguish between the velocity
(this is the force when playing)
and the volume.
The meaning of velocity depends on the instrument.

By digitizing sounds from the Yamaha SY-35
I found out that an increase of the MIDI volume by 16
doubles the amplitude, i.e. 16 steps correspond to 3 dB.
-}

import qualified Haskore.Interface.Signal.InstrumentMap as InstrMap
import qualified Haskore.Interface.Signal.Note as Note

import qualified Haskore.Music          as Music
import qualified Haskore.Music.Rhythmic as RhyMusic
import qualified Haskore.Performance as Performance
import qualified Haskore.Performance.Player  as Player
import qualified Haskore.Performance.Context as Context
import qualified Haskore.Performance.BackEnd as PerformanceBE
import qualified Haskore.Performance.Default as DefltPf
-- import qualified Haskore.Basic.Pitch as Pitch
import qualified Haskore.Basic.Tempo as Tempo

import qualified Data.EventList.Relative.TimeBody as EventList
-- import qualified Numeric.NonNegative.Class   as NonNeg
import qualified Number.NonNegative as NonNegW

import qualified Synthesizer.Plain.Signal as Sig
import qualified Synthesizer.Plain.Cut as Cut
import           Data.Tuple.HT (mapSnd, )

import qualified Algebra.Module    as Module
import qualified Algebra.RealField as RealField
import qualified Algebra.Ring      as Ring
import qualified Algebra.Additive  as Additive

import NumericPrelude.Base
import NumericPrelude.Numeric

import Prelude (RealFrac, Fractional, Floating, )



type Time   = Double
type Volume = Double
type NonNegTime = NonNegW.Double



{- | Convert a standard music into a list of numeric values
     that represent a mono or stereo audio signal. -}
fromRhythmicMusic ::
   (RealFrac time, Floating time, RealField.C time,
    Module.C dyn v, Fractional dyn, RealField.C dyn,
    Ord instr, Ord drum) =>
      time
   -> InstrMap.InstrumentTable time v instr
   -> (Player.Name -> Player.T (NonNegW.T time) dyn (RhyMusic.Note drum instr))
   -> Context.T (NonNegW.T time) dyn (RhyMusic.Note drum instr)
   -> RhyMusic.T drum instr
   -> Sig.T v
fromRhythmicMusic sampleRate instrMap =
   fromMusic sampleRate
      (Note.fromRhythmicNote undefined (InstrMap.lookup instrMap))


{- | Convert a generic music into an audio signal. -}
fromMusic ::
   (RealFrac time, Floating time, RealField.C time,
    Additive.C v, Ord dyn, Fractional dyn,
    Ord note) =>
      time
   -> Note.FromNote time dyn v note
   -> (Player.Name -> Player.T (NonNegW.T time) dyn note)
   -> Context.T (NonNegW.T time) dyn note
   -> Music.T note
   -> Sig.T v
fromMusic sampleRate noteMap pMap con =
   fromPerformance sampleRate noteMap .
      Performance.fromMusic pMap con


fromPerformance ::
   (RealFrac time, Floating time, RealField.C time,
    Additive.C v,
    Ord note) =>
      time
   -> Note.FromNote time dyn v note
   -> Performance.T (NonNegW.T time) dyn note
   -> Sig.T v
fromPerformance sampleRate noteMap =
   Cut.arrange .
   EventList.resample
      (NonNegW.fromNumberMsg "Signal.Write.fromPerformance" sampleRate) .
   EventList.mapBody (eventToPiece sampleRate) .
   PerformanceBE.fromPerformance noteMap


{- | Convert a generic note the sound of a single tone. -}
eventToPiece ::
   (RealFrac time, Floating time,
    RealField.C time) =>
      time
   -> PerformanceBE.Event (NonNegW.T time) (Note.T time v)
   -> Sig.T v
eventToPiece sampleRate event =
   let dur   = PerformanceBE.eventDur  event
       sound = PerformanceBE.eventNote event
   in  take
          (round (sampleRate * NonNegW.toNumber dur))
          (Note.toSignal sound sampleRate)

{- | can be used to turn an instrument mapper -}
detuneTone :: Ring.C time =>
      time
   -> (time -> time -> Sig.T v)
   -> (time -> time -> Sig.T v)
detuneTone detune noteToSignal =
   \ sampleRate freq ->
        noteToSignal sampleRate (freq * detune)

detuneInstrs :: Ring.C time =>
      time
   -> [(name, time -> time -> Sig.T v)]
   -> [(name, time -> time -> Sig.T v)]
detuneInstrs detune = map (mapSnd (detuneTone detune))

amplify :: Module.C a v =>
   a ->
   (time -> time -> Sig.T v) ->
   (time -> time -> Sig.T v)
amplify v sig sampleRate freq = map (v*>) (sig sampleRate freq)

{-
nonNegTimeInstr ::
   (time -> time -> Sig.T v) -> (NonNegW.T time -> NonNegW.T time -> Sig.T v)
nonNegTimeInstr f sampleRate freq =
   f (NonNegW.toNumber sampleRate) (NonNegW.toNumber freq)
-}


contextMetro :: Time -> Music.Dur -> Context.T NonNegTime Volume note
contextMetro setting dur =
   Context.setDur (Tempo.metro (NonNegW.fromNumberMsg "Signal.Write.contextMetro" setting) dur) $
   DefltPf.context
