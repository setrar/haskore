module Main where

import qualified Haskore.Interface.SuperCollider.Performance as SCPf
import qualified Haskore.Interface.SuperCollider.Note as SCNote

import Haskore.Music        hiding (repeat, reverse)
import Haskore.Melody          as Melody
import Haskore.Basic.Duration (wn, qn, en, (%+), )

import qualified Test.HUnit      as HUnit


propSuperColliderEventsFromNotes :: MidiMusic.T -> Bool
propSuperColliderEventsFromNotes music =
   let notes =
          PfBE.fromPaddedPerformance SCNote.fromGMNote $
          (FancyPerformance.paddedFromMusic music ::
                 Performance.Padded NonNegW.Rational Rational MidiMusic.Note)
   in  SCPf.fixNodeIds (SCPf.eventsFromNotesQueue notes) ==
       (SCPf.fixNodeIds (SCPf.eventsFromNotesEither notes)
             :: SCPf.T NonNegW.Rational)

testSuperColliderInfinitePerformance :: HUnit.Test
testSuperColliderInfinitePerformance =
   let -- mel = Music.repeat (a 0 wn () +:+ b 0 wn ())
       mel = a 0 wn () +:+ b 1 wn ()  =:=  rest qn +:+ mel
       pf = SCPf.fixNodeIds (SCPf.fromMusic SCNote.fromGMNote (withPiano mel))
                :: SCPf.T NonNegW.Double
   in  HUnit.TestCase
         (HUnit.assertBool "infinite performance"
            (testShowInf 200 pf))

testSuperCollider :: HUnit.Test
testSuperCollider =
   HUnit.TestLabel "supercollider" $ HUnit.TestList $
      -- fails for time of type Double probably because of rounding errors
      testUnit "eventsFromNotes" propSuperColliderEventsFromNotes :
      testSuperColliderInfinitePerformance :
      []


main :: IO ()
main = return ()
