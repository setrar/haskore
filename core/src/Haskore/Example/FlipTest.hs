module Main where

import qualified Haskore.Example.Flip as Flip
import qualified Haskore.Interface.MIDI.Render as RenderMidi
import qualified Haskore.Music.GeneralMIDI as MidiMusic
import qualified Sound.MIDI.File.Save as SaveMidi
import qualified Data.ByteString.Lazy as B

main :: IO ()
main =
   B.putStr $ SaveMidi.toByteString $ RenderMidi.mixedMidi $
   MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano Flip.song
