module Haskore.Example.BesondrerTag where

import           Haskore.Melody.Standard   as Melody
import           Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music             as Music


noAttr :: [Melody.NoteAttributes -> Melody.T] -> Melody.T
noAttr = line . map ($ na)

bar0, bar1, bass0, bass1 :: Melody.T
bar0 = noAttr $
   [b 0 qn,  g  0 qn, a 0 qn, d  1 en, c 1 en, b 0 qn, a 0 en, g 0 en, a 0 hn,
    g 0 dqn, fs 0 en, e 0 en, fs 0 en, g 0 qn, a 0 qn, b 0 qn, a 0 hn, g 0 hn]

bass0 = noAttr $
   [g 1 hn, d  1 hn, e 1 hn, d 1 hn, e 1 hn, c 1 hn, d  1 hn, d 1 hn, g 1 hn,
    g 1 hn, fs 1 hn, e 1 hn, d 1 hn, b 0 hn, c 1 hn, cs 1 hn, d 1 hn, g 1 hn]

bar1 = noAttr $
   [d 0 dqn, d 0 en, e 0 qn, fs 0 qn, g 0 qn, a 0 en, g 0 en, fs 0 qn, d 0 qn,
    g 0 dqn, g 0 en, a 0 qn, b  0 qn, c 1 qn, b 0 qn, a 0 hn, g  0 hn]

bass1 = noAttr $
   [d 1 hn, c  1 qn, a  0 qn, e 1 qn, cs 1 qn, d 1 qn, c 1 qn,
    b 0 hn, c  1 qn, cs 1 qn, d 1 hn, d  1 hn, g 1 hn]


melody :: Melody.T
melody = Music.replicate 2 bar0 +:+ bar1

bass :: Melody.T
bass = bass0 +:+ bass1

song :: MidiMusic.T
song =
   changeTempo 2
      (MidiMusic.fromStdMelody MidiMusic.AcousticGrandPiano (transpose ( 24) melody) =:=
       MidiMusic.fromStdMelody MidiMusic.StringEnsemble1    (transpose (-12) bass))
