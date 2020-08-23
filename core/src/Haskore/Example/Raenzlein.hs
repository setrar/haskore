module Haskore.Example.Raenzlein where

{- Heute wollen wir das Ränzlein schnüren -}

import           Haskore.Melody.Standard   as Melody
import           Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music             as Music
import qualified Haskore.Composition.Chord as Chord
import           Haskore.Basic.Pitch (Class(..))

import qualified Data.Accessor.Basic as Accessor


vline :: [NoteAttributes -> Melody.T] -> Melody.T
vline = line . map ($ Melody.na)

mel0 :: Melody.T
mel0 = vline
   [bf 0  en, d  1 en,
    f  1 dqn, f  1 en, f  1 en, f 1 en, g 1 en, a  1 en]

verse, refrain, strings :: Melody.T
verse =
   mel0 +:+
   vline
     [bf 1  hn, f  1 qn, g  1 en, f 1 en,
      f  1 dqn, ef 1 en, ef 1 en, g 1 en, f 1 en, ef 1 en,
      d  1  hn] +:+
   qnr +:+
   mel0 +:+
   vline
     [bf 1  qn, d  2 qn, d  2 qn, bf 1 en, d  2 en,
      c  2 dqn, a  1 en, c  2 en, bf 1 en, a 1 en, g 1 en,
      f  1  hn] +:+
   qnr

refrain =
   vline
     [f 1 den, g  1 sn, f 1 hn, ef 1 qn,
      g 1 den, a  1 sn, g 1 hn, f  1 qn,
      f 1  en, f  1 en, g 1 qn, f  1 qn, ef 1 qn, d 1 qn, c 1 hn] +:+
   qnr +:+
   Music.replicate 2 (vline
     [bf 0 en, d 1 en, f 1 dqn, g 1 en, f 1 qn,
      bf 1 en, a 1 en, g 1 dqn, a 1 en, g 1 qn,
      ef 2 en, ef 2 en, d 2 dqn, bf 1 en,
      c 2 den, c 2 sn, c 2 en, a 1 en, bf 1 hn]
   +:+ qnr)

melody :: Melody.T
melody = verse +:+ refrain



v :: NoteAttributes
v = Accessor.set Melody.velocity1 0.4 Melody.na


s1, s2 :: [Chord.Generic NoteAttributes]
s1 = 
  Chord.generic Bf Chord.majorInt           hn v :
  Chord.generic F  Chord.majorInt           hn v :
  Chord.generic Bf Chord.majorInt           wn v :
  Chord.generic F  Chord.dominantSeventhInt wn v :
  Chord.generic Bf Chord.majorInt          dwn v :
  Chord.generic F  Chord.majorInt           hn v :
  Chord.generic Bf Chord.majorInt           wn v :
  Chord.generic F  Chord.majorInt           hn v :
  Chord.generic C  Chord.dominantSeventhInt hn v :
  Chord.generic F  Chord.majorInt           wn v :
  Chord.generic F  Chord.dominantSeventhInt wn v :
  Chord.generic Bf Chord.majorInt           wn v :
  Chord.generic Ef Chord.majorInt           qn v :
  Chord.generic Bf Chord.majorInt           qn v :
  Chord.generic F  Chord.dominantSeventhInt qn v :
  Chord.generic Bf Chord.majorInt           qn v :
  Chord.generic F  Chord.majorInt           wn v :
  []
s2 =
  Chord.generic Bf Chord.majorInt           wn v :
  Chord.generic Ef Chord.majorInt           wn v :
  Chord.generic Bf Chord.majorInt           hn v :
  Chord.generic F  Chord.dominantSeventhInt hn v :
  Chord.generic Bf Chord.majorInt           wn v :
  []

strings = qnr +:+
  line (map chord
            (Chord.leastVaryingInversions
               ((1,C),(1,C))
               (s1 ++ s2 ++ s2)))


song :: MidiMusic.T
song =
   changeTempo (2)
      (MidiMusic.fromStdMelody MidiMusic.AcousticGrandPiano (transpose 24 melody) =:=
       MidiMusic.fromStdMelody MidiMusic.StringEnsemble1    (transpose 12 strings))
