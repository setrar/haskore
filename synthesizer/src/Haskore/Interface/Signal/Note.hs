{-# LANGUAGE NoImplicitPrelude #-}
module Haskore.Interface.Signal.Note where

import qualified Haskore.Interface.Signal.InstrumentMap as InstrMap

import qualified Haskore.Music.Rhythmic               as RhyMusic
import qualified Haskore.Basic.Pitch                  as Pitch

import qualified Synthesizer.Plain.Signal as Sig

-- import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module         as Module
import qualified Algebra.RealField      as RealField

-- import NumericPrelude.Base
import NumericPrelude.Numeric as NP

import qualified Prelude as P

newtype T time v = Cons {toSignal :: time -> Sig.T v}

{- |
tone signal renderer with parameters:
dynamics, transposition,
the note body (instrument, pitch, attributes)
-}
type FromNote time dyn v note =
           dyn   {- dynamics -}
        -> Pitch.Relative
                 {- transposition -}
        -> note  {- the note body containing instrument, pitch, attributes -}
        -> T time v


fromRhythmicNote ::
   (P.Floating time, RealField.C dyn, Module.C dyn v) =>
   InstrMap.ToDrum       time v drum ->
   InstrMap.ToInstrument time v instr ->
   FromNote time dyn v (RhyMusic.Note drum instr)
fromRhythmicNote dMap iMap dyn trans (RhyMusic.Note vel body) =
   -- for standard sounds we interpret the velocity simply as volume
   let velSig = velocityFromStd dyn vel
   in  case body of
          RhyMusic.Tone instr p ->
             Cons (\sampleRate -> velSig *>
                       iMap instr sampleRate (pitchFromStd trans p))
          RhyMusic.Drum drum ->
             Cons (\sampleRate -> velSig *> dMap drum sampleRate)

velocityFromStd :: RealField.C dyn =>
   dyn -> P.Rational -> dyn
velocityFromStd dyn vel = dyn * fromRational vel

pitchFromStd :: (P.Floating time) =>
   Pitch.Relative -> Pitch.T -> time
pitchFromStd trans p =
   Pitch.intToFreq (trans + Pitch.toInt p)
