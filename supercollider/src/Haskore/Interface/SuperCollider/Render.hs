module Haskore.Interface.SuperCollider.Render (
    fileFromMelody, fileFromMelodyMonad,

    byteStringFromSchedule, runSCSynth,
    HeaderFormat(..), SampleFormat(..),
  ) where

import qualified Haskore.Interface.SuperCollider.Schedule as Schedule
import qualified Haskore.Interface.SuperCollider.Play     as Play

import Haskore.Interface.SuperCollider.Schedule (Time)
import Haskore.Interface.SuperCollider.SoundMap (Instrument)

import qualified Sound.SC3.Server.NRT       as SCNRT
import qualified Sound.SC3.Server.PlayEasy  as SCPlay

-- import Sound.OpenSoundControl.Transport (Transport)
-- import qualified Sound.OpenSoundControl.Transport.Monad as Trans

import qualified Sound.OSC.Type as OSC
import qualified Sound.OSC.Time as OSCTime

import qualified Data.ByteString.Lazy as B

import qualified Haskore.Melody as Melody

import qualified Haskore.RealTime.Timer.Immediate as TimerImmediate
import qualified Haskore.RealTime.EventList.TimeBody as TimeList

import qualified Data.EventList.Absolute.TimeBody as AbsoluteEventList

import qualified Numeric.NonNegative.Wrapper as NonNeg

import System.Process (rawSystem)
import System.Exit (ExitCode)

import Data.Array(Array, Ix, (!), listArray)



{- * Generate audio files for various representations of music. -}

fileFromMelody :: FilePath -> Instrument -> Melody.T () -> IO ()
fileFromMelody fileName sound =
   B.writeFile fileName .
   byteStringFromSchedule .
   Schedule.fromMelody sound

fileFromMelodyMonad :: FilePath -> Instrument -> Melody.T () -> IO ()
fileFromMelodyMonad fileName sound =
   SCPlay.withSC3File fileName .
   Play.scheduleWithPlayer
      (Play.messagesGrouped TimerImmediate.timer 0) .
   Schedule.fromMelody sound


{- * invocation of SuperCollider program -}

data HeaderFormat = AIFF | Wave | NeXT
   deriving (Read, Show, Eq, Ord, Ix, Bounded)

headerExtension :: Array HeaderFormat String
headerExtension =
   listArray (minBound,maxBound) ["aiff", "wav", "au"]

headerName :: Array HeaderFormat String
headerName =
   listArray (minBound,maxBound) ["AIFF", "WAVE", "NeXT"]


data SampleFormat = Int16 | Int24 | Int32 | Float | Double
   deriving (Read, Show, Eq, Ord, Ix, Bounded)

sampleName :: Array SampleFormat String
sampleName =
   listArray (minBound,maxBound)
      ["int16", "int24", "int32", "float", "double"]

{-
because there are so much parameters,
an option list would be the better choice
-}
runSCSynth ::
   [String] ->
   Int {- ^ number of (stereo) channels -} ->
   HeaderFormat ->
   SampleFormat ->
   Int {- ^ sample rate -} ->
   FilePath ->
   IO ExitCode
runSCSynth
     options numChannels headerFormat sampleFormat sampleRate fileName =
   rawSystem "scsynth"
      (options ++
       ["-D", "0", "-o", show numChannels, "-N",
        fileName++".osc", "_", fileName ++ '.' : headerExtension ! headerFormat,
        show sampleRate, headerName ! headerFormat, sampleName ! sampleFormat])


{- * helper functions -}

byteStringFromSchedule :: Schedule.T -> B.ByteString
byteStringFromSchedule =
   SCNRT.encodeNRT . scheduleToStream

scheduleToStream ::
   Schedule.T ->
   SCNRT.NRT
scheduleToStream sc =
   SCNRT.NRT $
   timeStamp 0 (Schedule.initial sc) :
   messagesToStream (Schedule.body sc)

messagesToStream ::
   TimeList.T Time OSC.Message ->
   [OSC.Bundle]
messagesToStream =
   map (uncurry timeStamp) .
   AbsoluteEventList.toPairList .
   {- first absolutize, then collectCoincident in order to catch
      coincidences caused by rounding -}
   AbsoluteEventList.collectCoincident .
   TimeList.toAbsoluteEventList 0

timeStamp :: Time -> [OSC.Message] -> OSC.Bundle
timeStamp = OSC.Bundle . OSCTime.ut_to_ntpr . NonNeg.toNumber
