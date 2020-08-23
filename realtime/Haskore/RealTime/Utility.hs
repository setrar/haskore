module Haskore.RealTime.Utility where

import qualified System.Posix.Signals as Signals

import Control.Functor.HT (void, )


{- |
Disable sigPIPE.  This means that the whole program
won't crash when the tool exits.  Unfortunately there
doesn't seem to be another way of doing this.
-}
catchCtrlC :: IO ()
catchCtrlC =
   void $
   Signals.installHandler Signals.sigPIPE
      Signals.Ignore Nothing
