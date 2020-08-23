
module Haskore.Interface.SuperCollider.Note where

import qualified Haskore.Interface.SuperCollider.SoundMap as SoundMap

import Haskore.Interface.SuperCollider.SoundMap (Attribute)

import qualified Haskore.Music.Standard    as StdMusic
import qualified Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music.Rhythmic    as RhyMusic
import qualified Haskore.Basic.Pitch       as Pitch

import qualified Numeric.NonNegative.Wrapper as NonNeg

import qualified Data.Record.HT as Record
import Data.Ord.HT (comparing, )
import Data.Eq.HT  (equating, )


data T =
   Cons {
     parameters  :: [Attribute],
     velocity    :: NonNeg.Double,
     instrument  :: String,
     pitch       :: Maybe NonNeg.Double
   }
   deriving Show

{-
If the parameters would be pairs of parameter name and parameter value
then we could make use of the default values.
But is this worth the trouble?
-}

instance Eq T where
   (==) =
      Record.equal
         [equating pitch,
          equating velocity]

instance Ord T where
   compare =
      Record.compare
         [comparing pitch,
          comparing velocity]

type FromNote dyn note =
        dyn -> Pitch.Relative -> note -> T


fromRhythmicNoteWithAttributes :: (Real dyn) =>
   SoundMap.ToSound drum  ->
   SoundMap.ToSound instr ->
   FromNote dyn (RhyMusic.Note drum instr)
fromRhythmicNoteWithAttributes dMap iMap dyn trans (RhyMusic.Note vel body) =
   let velSC = velocityFromStd dyn vel
   in  case body of
          RhyMusic.Tone instr p ->
             uncurry (flip Cons velSC) (iMap instr)
                     (Just (pitchFromStd trans p))
          RhyMusic.Drum drum ->
             uncurry (flip Cons velSC) (dMap drum) Nothing

fromRhythmicNote :: (Real dyn) =>
   (drum  -> String) ->
   (instr -> String) ->
   FromNote dyn (RhyMusic.Note drum instr)
fromRhythmicNote dMap iMap =
   fromRhythmicNoteWithAttributes
      (\drum  -> ([], dMap drum))
      (\instr -> ([], iMap instr))

fromGMNote :: (Real dyn) =>
   FromNote dyn MidiMusic.Note
fromGMNote = fromRhythmicNote show show

fromStdNote :: (Real dyn) =>
   FromNote dyn StdMusic.Note
fromStdNote = fromRhythmicNote id id

velocityFromStd :: Real dyn =>
   dyn -> Rational -> NonNeg.Double
velocityFromStd dyn vel = realToFrac dyn * fromRational vel

{-
The resulting frequency is always positive,
but we must explicitly convert to NonNeg.T
because we cannot perform the whole computation in NonNeg.Double
because the pitch can be negative.
-}
pitchFromStd ::
   Pitch.Relative -> Pitch.T -> NonNeg.Double
pitchFromStd trans p =
   NonNeg.fromNumberMsg "SuperCollider.Note.pitchFromStd" $
   Pitch.intToFreq (trans + Pitch.toInt p)
