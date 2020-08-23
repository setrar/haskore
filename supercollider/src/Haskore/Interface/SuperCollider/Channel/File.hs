{- |
An instance of 'Haskore.Interface.SuperCollider.Channel.T'
using a temporary file.
-}
module Haskore.Interface.SuperCollider.Channel.File where

import qualified Haskore.Interface.SuperCollider.Channel as ChannelMng
import Haskore.Interface.SuperCollider.Channel
          (Channel, NumberChannels)

import System.IO.Error (catchIOError)

-- import System.Directory (removeFile)
import Data.Char (isDigit)


manager :: ChannelMng.T IO
manager = ChannelMng.Cons next reset


next :: NumberChannels -> IO Channel
next inc =
   do num <- catchIOError
         (do numStr <- readFile fileName
             if all isDigit numStr
               then return (read numStr)
               else ioError (userError "not a number in channel file"))
         (const $ return ChannelMng.least)
      -- removeFile fileName
      writeFile fileName (show (inc+num))
      return num

reset :: IO ()
reset = writeFile fileName (show ChannelMng.least)

fileName :: FilePath
fileName = "sc3-channel"
