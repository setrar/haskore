module Haskore.Example.Flip where

import Haskore.Melody as Melody
import Haskore.Music.GeneralMIDI as MidiMusic

import Data.Array (Array, (!), listArray)
import qualified Data.List as List

{-
  flipSeq 2 !! n = parity of number of 1's in binary representation of n.
  http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=A010060
-}

flipSeq :: Int -> [Int]
flipSeq n =
   let incList m = map (\x -> mod (x+m) n)
       recourse y = let z = concatMap (flip incList y) [1..(n-1)]
                   in  z ++ recourse (y++z)
   in  [0] ++ recourse [0]

{- based on Helmut Podhaisky's implementation
   it must be flipSeq2 == flipSeq 2 -} 
flipSeq2 :: [Int]
flipSeq2 =
   let recourse y = let z = map (1-) y
                   in  z ++ recourse (y++z)
   in  [0] ++ recourse [0]


noteArray :: [() -> Melody.T ()] -> Array Int (Melody.T ())
noteArray ns = listArray (0, length ns - 1) (map (\n -> n ()) ns)

makeSong :: [() -> Melody.T ()] -> Melody.T ()
makeSong ms = line (map (noteArray ms ! )
                            (flipSeq (length ms)))

song, song1, core, core1 :: Melody.T ()

song = changeTempo 8 core
core = makeSong [e 1 qn, g 1 qn, c 2 qn, e 2 qn]

song1 = changeTempo 8 core1
core1 =
   let rep = 16
   in  line $ zipWith (!) (cycle
          (List.replicate rep (noteArray [e 1 qn, a 1 qn, c 2 qn, e 2 qn]) ++
           List.replicate rep (noteArray [g 1 qn, c 2 qn, e 2 qn, g 2 qn]) ++
           List.replicate rep (noteArray [a 1 qn, d 2 qn, f 2 qn, a 2 qn]) ++
           List.replicate rep (noteArray [a 1 qn, c 2 qn, f 2 qn, a 2 qn]) ++
           List.replicate rep (noteArray [a 1 qn, c 2 qn, e 2 qn, a 2 qn])))
          (flipSeq 4)

{-
  If you divide the stream into blocks of size n
  each block will contain each of the indices of {0,..,n-1} exactly once.
  Thus you can also choose musical atoms of different length
  for generating rythms.
-}
song2, core2 :: MidiMusic.T
song2 = changeTempo 4 core2
core2 =
   let rep = 16
       flipper = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano $
         line $ zipWith (!) (cycle
          (List.replicate rep (noteArray [e 1 dqn, a 1 en, c 2 qn, e 2 qn]) ++
           List.replicate rep (noteArray [g 1 dqn, c 2 en, e 2 qn, g 2 qn]) ++
           List.replicate rep (noteArray [a 1 dqn, d 2 en, f 2 qn, a 2 qn]) ++
           List.replicate rep (noteArray [a 1 dqn, c 2 en, f 2 qn, a 2 qn]) ++
           List.replicate rep (noteArray [a 1 dqn, c 2 en, e 2 qn, a 2 qn]) ++
           List.replicate rep (noteArray [a 1 dqn, c 2 en, e 2 qn, a 2 qn])))
          (flipSeq 4)
       bassLine =
          MidiMusic.fromMelodyNullAttr MidiMusic.Viola $
          transpose (-12) $ line $ cycle $
          concatMap (List.replicate 8) $
          List.map ($ ())
             [a 0 hn, c 1 hn, d 1 hn,
              f 1 hn, a 1 hn, a 0 hn]
   in  flipper =:= bassLine
