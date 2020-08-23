{- |
Christman song "O Heiland, reiß die Himmel auf"
from "Du wurdest meine Sonne - Heft I:
Advents- und Weihnachtslieder in einfachen Sätzen"
Evangelische Verlagsanstalt Berlin

fileFromGeneralMIDIMusic "heiland.mid" song
-}
module Haskore.Example.HeilandHimmel where

import           Haskore.Melody.Standard   as Melody
import           Haskore.Music.GeneralMIDI as MidiMusic


noAttr :: [Melody.NoteAttributes -> Melody.T] -> Melody.T
noAttr = line . map ($ na)

melody0, melody1, melody2, melody3,
  bass0, bass1, bass2, bass3 :: Melody.T
melody0 =
   d 0 qn na +:+
   (d 0 hn na =:= f 0 hn na) +:+
   (e 0 qn na =:= g 0 qn na) +:+
   ((f 0 qn na +:+ e 0 qn na) =:= a 0 hn na) +:+
   d 0 qn na +:+
   (d 0 hn na +:+ cs 0 qn na =:= f 0 qn na +:+ e 0 hn na) +:+
   d 0 dhn na

bass0 = noAttr $
   [d  1 qn, d  1 qn, c  1 qn, bf 0 qn,
    a  0 hn, bf 0 qn, bf 0 qn, g  0 qn, a  0 qn,
    d  0 qn, a  0 qn, d  1 qn]

melody1 =
   (f 0 qn na =:= a 0 qn na) +:+
   (e 0 qn na =:= a 0 qn na) +:+
   (d 0 qn na =:= b 0 qn na) +:+
   ((c 0 qn na +:+ d 0 qn na) =:= c 1 hn na) +:+
   f 0 qn na +:+
   (f 0 hn na +:+ e 0 qn na =:= a 0 qn na +:+ g 0 hn na) +:+
   f 0 dhn na

bass1 = noAttr $
   [d  1 qn, c  1 qn, b  0 qn,
    a  0 hn, d  1 qn, d  1 qn, bf 0 qn, c  1 qn,
    f  0 qn, c  1 qn, f  1 qn]

melody2 =
   (g 0 hn na =:= c 1 qn na +:+ c 1 qn na) +:+
   (f 0 qn na =:= c 1 qn na) +:+
   (e 0 qn na +:+ g 0 qn na =:= c 1 hn na) +:+
   a 0 qn na +:+
   (a 0 qn na +:+ g 0 qn na =:= d 1 hn na) +:+
   (f 0 qn na =:= d 1 qn na) +:+
   (f 0 qn na +:+ d 0 qn na +:+ e 0 qn na =:= c 1 qn na)

bass2 = noAttr $
   [f  1  qn, e  1 qn, d  1 qn, c  1 qn, e  1 qn, f  1 qn,
    bf 0 dhn, c  1 qn, g  0 qn, c  0 qn]

melody3 =
   (f 0 qn na =:= a 0 qn na) +:+
   (e 0 qn na =:= a 0 qn na) +:+
   (d 0 qn na =:= g 0 qn na) +:+
   (d 0 qn na +:+ cs 0 qn na =:= a 0 hn na) +:+
   (d 0 qn na =:= f 0 qn na) +:+
   (d 0 hn na +:+ cs 0 qn na =:= g 0 qn na +:+ e 0 hn na) +:+
   d 0 qn na

bass3 = noAttr $
   [d  0 hn, e  0 qn, a  0 hn, bf 0 qn,
    bf 0 qn, g  0 qn, a  0 qn, d  0 hn]


melody :: Melody.T
melody = melody0 +:+ melody1 +:+ melody2 +:+ melody3

bass :: Melody.T
bass = bass0 +:+ bass1 +:+ bass2 +:+ bass3

song :: MidiMusic.T
song =
   changeTempo 1.5 $
      MidiMusic.fromStdMelody MidiMusic.PercussiveOrgan (transpose ( 12) melody)
      =:=
      MidiMusic.fromStdMelody MidiMusic.StringEnsemble1 (transpose (-12) bass)
