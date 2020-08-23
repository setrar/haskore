-- | replace functionality of shell-pipe package
module Haskore.RealTime.ShellPipe (launch) where

import System.Process(runInteractiveProcess)
import System.IO (Handle, )

launch ::
       FilePath  {- ^ shell command name -}
   -> [String]   {- ^ raw arguments -}
   -> IO (Handle, Handle, Handle)
                 {- ^ streams for input, output, error -}
launch cmd args =
   fmap
      (\(input,output,err,_) -> (input,output,err))
      (runInteractiveProcess cmd args Nothing Nothing)
