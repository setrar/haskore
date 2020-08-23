module Haskore.RealTime.Timer.Thread (timer) where

import qualified Haskore.RealTime.Timer as Timer
import System.Time(getClockTime)
import Control.Concurrent(threadDelay)

import Control.Monad(when)

import qualified Numeric.NonNegative.Wrapper as NonNeg


timer :: Timer.T IO
timer = Timer.Cons getClockTime wait resolution


{-
threadDelay always waits a minimum of 1/50 second.
It seems to round up, even if we pass a multiple of 1/50 seconds.
We have to work-around that.
-}

{- |
Wait the given number of @(recip resolution)@ seconds.
-}
wait :: NonNeg.Int -> IO ()
wait delay =
   when (delay > 0)
        (threadDelay $ NonNeg.toNumber (pred delay * unit))

resolution :: Num a => a
resolution = 50

unit :: NonNeg.Int
unit = div (10^(6::Int)) resolution
