{- |
Apply actions to event lists
(starting with time, ending with body)
at given times.
-}
module Haskore.RealTime.EventList.TimeBody
   (T,
    mapBodyM, mapM, mapM_, getBodies,
    mapBody, mapTime,
    mapTimeTail,
    empty, singleton, null, viewL, viewTimeL, viewBodyL, cons, snoc,
    consBody, consTime,
    append, concat, cycle,
    insert, decreaseStart, delay, partition, foldr, slice, normalize,
    collectCoincident, flatten, mapCoincident,
    resample, toAbsoluteEventList,
    run, runTimeStamp, runTimeStampGrouped,
    runCore, runRelative, runRelativeCore,
    attachTime,
   ) where

import Data.EventList.Relative.TimeBody
import Data.EventList.Relative.MixedBody
-- import qualified Data.EventList.Relative.BodyBody as BodyBody
import qualified Data.EventList.Absolute.TimeBody as AbsList

import qualified Haskore.RealTime.Timer as Timer
import qualified Numeric.NonNegative.Class as NonNeg
import qualified Numeric.NonNegative.Wrapper as NonNegW

import qualified Control.Monad as Monad

import Prelude hiding (null, foldr, mapM, mapM_, concat, cycle, )


-- * Run actions according to an event list

{- |
The first function assumes,
that the action does not consume time
and that the wait command is precise.
It is not very useful in practice, but very simple.
-}

runRelative :: (NonNeg.C time, RealFrac time, Monad m) =>
   Timer.T m -> (body -> m a) -> T time body -> m [a]
runRelative timer action =
   runRelativeCore timer action .
   resample (fromIntegral $ Timer.resolution timer)

runRelativeCore :: Monad m =>
   Timer.T m -> (body -> m a) -> T NonNegW.Integer body -> m [a]
runRelativeCore timer action =
   Monad.liftM getBodies .
   mapM (Timer.wait timer) action

{- |
The next set of routines is more precise.
It computes a time table starting with current system time
and tries to stick to it.

So far, I worked hard to use time differences instead of absolute times
in order to avoid increasing memory consumption of time numbers
(which however slows down as time evolves)
but the time related functions of the system are absolute,
so have to make our ones absolute as well.
-}

run :: (RealFrac time, Monad m) =>
   Timer.T m -> (body -> m a) -> T time body -> m [a]
run timer action = runTimeStamp timer (const action)

{- |
The wait calls are necessarily quantized,
but the time passed to the action is not quantized.
-}

runTimeStamp :: (RealFrac time, Monad m) =>
   Timer.T m -> (time -> body -> m a) -> T time body -> m [a]
runTimeStamp =
   runCore id

{- |
This routine is only necessary,
because differences might be too small
to be noticed in the absolute time values.
That is, collectCoincident will split events which actually belong together.
-}

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

{- |
We export this function only for use in "Haskore.RealTime.EventList.TimeTime".
-}
attachTime :: AbsList.T time body -> AbsList.T time (time, body)
attachTime =
   AbsList.fromPairList .
   map (\ ~(time,body) -> (time, (time,body))) .
   AbsList.toPairList
