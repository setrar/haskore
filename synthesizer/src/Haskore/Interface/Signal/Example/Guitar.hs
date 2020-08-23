module Haskore.Interface.Signal.Example.Guitar where

-- the song
import Haskore.Example.Guitar (parallelSong, stringPitches, )
-- the instrument
import Synthesizer.Filter.Example (guitar, )

import qualified Haskore.Basic.Pitch as Pitch
import           Haskore.Music.Standard  as StdMusic
import           Haskore.Music.Rhythmic  as RhyMusic
import qualified Haskore.Performance.Fancy as FancyPf
import qualified Haskore.Interface.Signal.InstrumentMap as InstrMap
import qualified Haskore.Interface.Signal.Write as MusicSignal
import Haskore.Interface.Signal.Write(Time,Volume)

import qualified Synthesizer.Plain.Filter.NonRecursive as FiltNR
import qualified Synthesizer.Plain.Interpolation as Interpolation
import qualified Synthesizer.Plain.Signal as Sig
import           Synthesizer.Plain.Instrument
          (fastBell, simpleSaw, moogGuitar, moogGuitarSoft, fmBell, )
import qualified Synthesizer.Plain.File as File

import           Control.Monad.HT (void, )
import           Data.Tuple.HT (mapSnd, )

import System.Exit(ExitCode)



stringFreqs :: [Time]
stringFreqs =
   map (\p -> Pitch.intToFreq (Pitch.toInt p) / 48000) stringPitches


sampleSong :: RhyMusic.T () Int
sampleSong = parallelSong [0 .. 5]

synthSong :: StdMusic.T
synthSong =
   StdMusic.transpose 12 (parallelSong (repeat "moogguitarsoft"))



----------- Configuration of the player -----------

type IMap instr = InstrMap.InstrumentTable Time Volume instr

guitarToSignal :: Time -> Sig.T Volume -> InstrMap.Instrument Time Volume
guitarToSignal stringFreq sound sampleRate freq =
   Interpolation.multiRelativeZeroPadConstant 0
      (repeat (freq/sampleRate / stringFreq)) sound

sampleMap :: [Sig.T Volume] -> IMap Int
sampleMap samples =
   zipWith3 (\chr stringFreq sound ->
               (chr, guitarToSignal stringFreq sound))
            [0 ..] stringFreqs samples

synthMap :: IMap StdMusic.Instr
synthMap = map (mapSnd (MusicSignal.amplify (1/5::Volume)))
  [("guitar",         (\sampleRate freq -> guitar (freq/sampleRate))),
   ("bell",           fastBell),
   ("string",         simpleSaw),
   ("moogguitar",     moogGuitar),
   ("moogguitarsoft", moogGuitarSoft),
   ("fmguitar",       (\sampleRate -> fmBell sampleRate 0.4 3.003))]

computeSignal :: (Ord drum, Ord instr) =>
   RhyMusic.T drum instr -> IMap instr ->
                    Time -> Sig.T (Volume,Volume)
computeSignal music instrMap sampleRate =
   let channel dif =
          MusicSignal.fromRhythmicMusic sampleRate
             (MusicSignal.detuneInstrs dif instrMap)
             FancyPf.map
             (MusicSignal.contextMetro 60 qn)
             music
   in  zip (channel 1.001) (channel 0.999)

readSamples :: IO [Sig.T Volume]
readSamples =
   do sampledSounds <- mapM File.readMonoFromInt16
             (map (\chr -> "guitar/Zupf/Zupf"++chr:".aiff") ['0'..'5'])
      return (zipWith FiltNR.amplifyVector
                      [0.5,0.5,0.5,0.5,0.5,0.3::Volume]
                      sampledSounds)

main :: IO ExitCode
main =
   do
     void $
      File.renderStereoToInt16 "Guitar.aiff" 44100 . computeSignal sampleSong . sampleMap
        =<< readSamples
     File.renderStereoToInt16 "GuitarSynth.aiff" 44100 (computeSignal synthSong synthMap)
