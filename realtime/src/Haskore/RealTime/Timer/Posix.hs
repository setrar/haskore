module Haskore.RealTime.Timer.Posix (timer) where

import qualified Haskore.RealTime.Timer as Timer
import System.Time(getClockTime)
import System.Posix.Unistd(usleep)

import qualified Numeric.NonNegative.Wrapper as NonNeg


timer :: Timer.T IO
timer = Timer.Cons getClockTime wait resolution


{-
sleep can be aborted with CTRL-C, but measures in seconds.
usleep measures in micro-seconds, but is unbreakable.
Non is appropriate.
-}
wait :: NonNeg.Int -> IO ()
wait delay =
   -- putStrLn ("wait " ++ show delay) >>
   -- usleep (delay*unit)
   usleep (NonNeg.toNumber delay)
   -- sleep delay >> return ()

resolution :: Num a => a
resolution = 10^(6::Int)

{-
unit :: Int
unit = div (10^(6::Int)) resolution
-}
