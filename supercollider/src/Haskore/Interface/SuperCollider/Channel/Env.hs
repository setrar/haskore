{- |
An instance of 'Haskore.Interface.SuperCollider.Channel.T'
using environment variables.
Since we need writing of variables,
we need "System.Posix.Env" module
rather than the generic "System.Environment".
-}
module Haskore.Interface.SuperCollider.Channel.Env where

import qualified Haskore.Interface.SuperCollider.Channel as ChannelMng
import Haskore.Interface.SuperCollider.Channel
          (Channel, NumberChannels)

import System.Posix.Env (setEnv, getEnv)
import Data.Char (isDigit)


manager :: ChannelMng.T IO
manager = ChannelMng.Cons next reset


next :: NumberChannels -> IO Channel
next inc =
   do maybeNumStr <- getEnv envName
      let num =
            maybe ChannelMng.least
               (\numStr ->
                  if all isDigit numStr
                    then read numStr
                    else ChannelMng.least)
               maybeNumStr
      setEnv envName (show (inc+num)) True
      return num

reset :: IO ()
reset = setEnv envName (show ChannelMng.least) True

envName :: String
envName = "HaskoreSC3Channel"
