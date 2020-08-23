{- |
Apply actions to event lists
(starting with time, ending with time)
at given times.
-}

module Haskore.RealTime.EventList.TimeTime
   (T,
    mapBody, mapTime, mapM, mapM_,
    BodyTimeList.empty, pause,
    merge, insert, decreaseStart, delay, filter, foldr,
    viewTimeL, viewBodyL, viewTimeR, viewBodyR, {- viewR, -}
    cons, consBody, consTime,
    snoc, snocBody, snocTime,
    mapTimeL, mapTimeHead, mapTimeTail,
    mapBodyL, mapBodyHead, mapBodyTail,
    mapTimeR, mapTimeLast, mapTimeInit,
    mapBodyR, mapBodyLast, mapBodyInit,
    catMaybes,
    append,
    concat, concatNaive, resample,
    toAbsoluteEventList,
    run, runTimeStamp, runTimeStampGrouped, runRelative,
    collectCoincident, flatten, mapCoincident,
   ) where

import Data.EventList.Relative.TimeTime
import Data.EventList.Relative.MixedTime
import Data.EventList.Relative.TimeMixed
import qualified Data.EventList.Relative.BodyTime as BodyTimeList
import qualified Data.EventList.Absolute.TimeTime as AbsList
import qualified Data.EventList.Absolute.TimeMixed as AbsListMix

import qualified Haskore.RealTime.Timer as Timer
import qualified Haskore.RealTime.EventList.TimeBody as TimeList
import qualified Numeric.NonNegative.Class as NonNeg
-- import qualified Numeric.NonNegative.Wrapper as NonNegW

import qualified Control.Monad as Monad

import Prelude hiding (concat, filter, foldr, mapM, mapM_)



runRelative :: (NonNeg.C time, RealFrac time, Monad m) =>
   Timer.T m -> (body -> m a) -> T time body -> m [a]
runRelative timer action =
   Monad.liftM getBodies .
   mapM (Timer.wait timer) action .
   resample (fromIntegral $ Timer.resolution timer)


run :: (RealFrac time, Monad m) =>
   Timer.T m -> (body -> m a) -> T time body -> m [a]
run timer action = runTimeStamp timer (const action)

runTimeStamp :: (RealFrac time, Monad m) =>
   Timer.T m -> (time -> body -> m a) -> T time body -> m [a]
runTimeStamp =
   runCore id

runTimeStampGrouped :: (RealFrac time, Monad m) =>
   Timer.T m -> (time -> [body] -> m a) -> T time body -> m [a]
runTimeStampGrouped =
   runCore AbsList.collectCoincident

runCore :: (Fractional time0, RealFrac time1, Monad m) =>
   (AbsList.T time0 body0 -> AbsList.T time1 body1) ->
   Timer.T m -> (time1 -> body1 -> m a) -> T time0 body0 -> m [a]
runCore convertAbs timer action evs =
   Monad.liftM AbsList.getBodies .
   AbsList.mapM
      (Timer.waitUntilSeconds timer)
      (uncurry action) .
   attachTime .
   convertAbs .
   flip toAbsoluteEventList evs =<<
   Timer.getTimeSeconds timer

attachTime :: AbsList.T time body -> AbsList.T time (time, body)
attachTime =
   AbsListMix.mapTimeInit TimeList.attachTime




{-
*Haskore.RealTime.EventList.TimeTime Haskore.RealTime.Timer.Thread> runRelative Haskore.RealTime.Timer.Thread.timer print $ 0.1 /. 'a' ./ (0.1::Numeric.NonNegative.Wrapper.Double) /. 'b' ./ undefined

*Haskore.RealTime.EventList.TimeTime Haskore.RealTime.Timer.Thread> runTimeStamp Haskore.RealTime.Timer.Thread.timer (\t b -> print (t,b)) $ 0.1 /. 'a' ./ 0.1 /. 'b' ./ undefined
(1.1966023807219398e9,'a')
(1.1966023808219397e9,*** Exception: Prelude.undefined
-}
