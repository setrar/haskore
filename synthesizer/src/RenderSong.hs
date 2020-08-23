module Main where

import qualified Synthesizer.Plain.Play as Play
import qualified Haskore.Interface.Signal.Example.WinterAde as Song

import Control.Monad.HT (void)


main :: IO ()
main = void $ Play.stereoToInt16 11025 Song.songSignal
