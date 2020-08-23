module Haskore.Interface.SuperCollider.Timer where

import qualified Haskore.RealTime.Timer           as Timer
import qualified Haskore.RealTime.Timer.Thread as TimerThread

import Control.Monad.IO.Class (MonadIO, )


timer :: MonadIO io => Timer.T io
timer = Timer.liftIO TimerThread.timer
