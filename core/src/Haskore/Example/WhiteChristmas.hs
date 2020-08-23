
{- Demonstrate handling of chords and drums -}

module Haskore.Example.WhiteChristmas where

import qualified Haskore.Composition.Drum  as Drum
import qualified Haskore.Composition.Chord as Chord
import           Haskore.Basic.Dynamics (Velocity)
import           Haskore.Melody.Standard   as Melody
import           Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music             as Music
import           Haskore.Basic.Pitch (Class(..))

import qualified Data.Accessor.Basic as Accessor


vline :: [NoteAttributes -> Melody.T] -> Melody.T
vline l = line (map ($ Melody.na) l)

melody, strings :: Melody.T
melody  = line [m1, m2, m3a, m4a,   m1, m2, m3b, m4b]
strings = line (map chord
            (Chord.leastVaryingInversions
               ((1,C),(1,C))
               (s1 ++ s2 ++ s3 ++ s4a   ++   s1 ++ s2 ++ s3 ++ s4b)))

m1, m2, m3a, m4a, m3b, m4b :: Melody.T
m1 = vline [e  1  hn, f  1  en, e  1  en, ds  1  en, e  1  en,
            f  1  hn, fs 1  en, g  1 dqn]
m2 = enr +:+
     vline [a  1  en, b  1  en, c  2  en,
            d  2  en, c  2  en, b  1  en, a  1  en,
            g  1  hn]
m3a = qnr +:+  vline [c  1  en, d  1  en, e  1  qn, e  1  qn,
            e  1  en, a  1  qn, g  1  en, c  1  qn, c  1  qn,
            c  1  en, g  1  qn]
m4a = vline [f  1  en, e  1  hn, f  1  en, e  1  en,
             d  1  en, c  1  en, d  1  hn, g  0  hn]
m3b = qnr +:+  vline [c  1  en, d  1  en, e  1  qn, e  1  qn,
            e  1  en, a  1  qn, g  1  en, c  2 dhn]
m4b = vline [c  1  en, d  1  en, e  1  qn, e  1  qn, a  1  en,
             g  1  en, a  0  en, b  0  en, c  1  hn]

v :: NoteAttributes
v = vel 0.25

vel :: Velocity -> NoteAttributes
vel vl = Accessor.set Melody.velocity1 vl Melody.na

s1, s2, s3, s4a, s4b :: [Chord.Generic NoteAttributes]
s1 = [
       Chord.generic C Chord.majorInt           wn v,
       Chord.generic D Chord.minorInt           hn v,
       Chord.generic G Chord.majorInt           hn v
     ]
s2 = [
       Chord.generic F Chord.majorInt           wn v,
       Chord.generic C Chord.majorInt           hn v,
       Chord.generic G Chord.sustainedFourthInt qn v,
       Chord.generic G Chord.majorInt           qn v
     ]
s3 = [
       Chord.generic C Chord.majorInt           qn v,
       Chord.generic E Chord.minorInt           qn v,
       Chord.generic C Chord.dominantSeventhInt hn v,
       Chord.generic F Chord.majorInt           hn v,
       Chord.generic F Chord.minorInt           hn v
     ]
s4a =[
       Chord.generic C Chord.majorInt           hn v,
       Chord.generic D Chord.minorInt           qn v,
       Chord.generic D Chord.majorInt           qn v,
       Chord.generic G Chord.sustainedFourthInt hn v,
       Chord.generic G Chord.majorInt           hn v
     ]
s4b =[
       Chord.generic C Chord.majorInt           hn v,
       Chord.generic G Chord.majorInt           hn v,
       Chord.generic C Chord.majorInt           hn v
     ]


bassdrum, snare, hihat :: Dur -> MidiMusic.T
bassdrum durat = Drum.toMusic MidiMusic.AcousticBassDrum durat (vel 2)
snare    durat = Drum.toMusic MidiMusic.AcousticSnare    durat (vel 1)
hihat    durat = Drum.toMusic MidiMusic.OpenHiHat        durat (vel 1.5)

rhythm :: MidiMusic.T
rhythm =
   line [bassdrum en, hihat sn, hihat sn,
         snare en,    hihat sn, hihat sn,
         bassdrum en, hihat sn, hihat sn,
         snare sn,    hihat sn, hihat sn, hihat sn]

song :: MidiMusic.T
song = MidiMusic.changeTempo 1.2 $
   MidiMusic.fromStdMelody MidiMusic.StringEnsemble1
          (transpose 12 strings) =:=
   MidiMusic.fromStdMelody MidiMusic.AcousticGrandPiano
          (transpose 12 melody) =:=
   Music.line (replicate 16 rhythm)
