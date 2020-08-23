{- |
This timer doesn't wait but returns immediately.
This is useful for turning real-time processing to non-real-time processing.
-}
module Haskore.RealTime.Timer.Immediate (timer) where

import qualified Haskore.RealTime.Timer as Timer
import System.Time(ClockTime(TOD))

import qualified Numeric.NonNegative.Wrapper as NonNeg


timer :: (Monad m) => Timer.T m
timer = Timer.Cons getClockTime wait resolution

getClockTime :: (Monad m) => m ClockTime
getClockTime = return $ TOD 0 0

{- |
Wait the given number of @(recip resolution)@ seconds.
-}
wait :: (Monad m) => NonNeg.Int -> m ()
wait _ = return ()

resolution :: Num a => a
resolution = 50
