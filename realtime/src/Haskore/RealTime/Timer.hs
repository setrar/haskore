module Haskore.RealTime.Timer where

import System.Time (ClockTime(TOD))
import Control.Monad (liftM, replicateM_, )

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.IO.Class as MIO

import qualified Numeric.NonNegative.Wrapper as NonNeg
-- import Numeric.NonNegative.Class ((-|))


data T m = Cons {
     getClockTime :: m ClockTime,
     waitInt      :: NonNeg.Int -> m (),
     resolution   :: NonNeg.Integer
  }

lift :: (Trans.MonadTrans t, Monad m) => T m -> T (t m)
lift (Cons g w r) = Cons (Trans.lift g) (Trans.lift . w) r

liftIO :: MIO.MonadIO io => T IO -> T io
liftIO (Cons g w r) = Cons (MIO.liftIO g) (MIO.liftIO . w) r


getTime :: (Monad m) => T m -> m Integer
getTime timer =
   liftM (clockTimeToWaitTime (resolution timer)) $
   getClockTime timer

clockTimeToWaitTime :: NonNeg.Integer -> ClockTime -> Integer
clockTimeToWaitTime res0 (TOD secs picos) =
   let res = NonNeg.toNumber res0
   in  secs * res + div (picos * res) (10^(12::Int))

getTimeSeconds :: (Fractional time, Monad m) =>
   T m -> m time
getTimeSeconds timer =
   liftM clockTimeToSeconds $
   getClockTime timer

clockTimeToSeconds :: Fractional time => ClockTime -> time
clockTimeToSeconds (TOD secs picos) =
   fromInteger secs + fromInteger picos * 1e-12

{-
The range of 32 bit Ints does not suffice for waiting an hour
measured in microseconds.
-}
wait :: (Monad m) => T m -> NonNeg.Integer -> m ()
wait timer time =
   let blockSize = maxBound
       {- Negative delays can occur if multiple events should be scheduled
          at the same time, but are processed subsequently.
          In this case we just not wait but hope to return to schedule
          as time goes by. -}
       (reps,remainder) = divMod (max 0 time) (fromIntegral blockSize)
   in  -- putStrLn ("wait Integer " ++ show time) >>
       -- print (reps,remainder) >>
       --  if time<0
       --    then error "Timer.wait: can't wait a negative time"

       -- I hope that 'reps' will always fit in Int range
       replicateM_ (fromIntegral reps) (waitInt timer blockSize) >>
       waitInt timer (fromIntegral remainder)

waitUntil :: (Monad m) => T m -> Integer -> m ()
waitUntil timer time =
   do tcur <- getTime timer
      wait timer (NonNeg.fromNumberClip (time - tcur))

waitUntilSeconds :: (RealFrac time, Monad m) =>
   T m -> time -> m ()
waitUntilSeconds timer time =
   waitUntil timer (floor (time * fromIntegral (resolution timer)))
