{- |
This module uses a pipe
in order to play CSound music without a temporary score file.
A temporary orchestra file is necessary, though.

In my tests on a SuSE Linux only CSound5 (but not CSound4)
could play via '-odac'.
-}
module Haskore.Interface.CSound.Play where

import Haskore.RealTime.ShellPipe (launch)
import Haskore.RealTime.Utility (catchCtrlC, )

import qualified System.IO as IO
import System.Process (rawSystem, system, )
import System.Directory(doesFileExist, removeFile, )

import           Haskore.Interface.CSound (Name, )
import qualified Haskore.Interface.CSound.Orchestra as Orchestra
import qualified Haskore.Interface.CSound.Score     as Score

import Control.Functor.HT (void, )


play :: Orchestra.Output out => Orchestra.T out -> Score.T -> IO ()
play orc sco = playV4 "/tmp" ("tmp", orc, sco)

scorePipe, tmpWave :: FilePath
scorePipe = "/tmp/pipe.sco"
tmpWave   = "/tmp/csound.wav"


{-
  This works:
  cat tut01.sco >plug & csound32 -d -o stdout tut01.orc plug | play -t sw -r 44100 -c 2 -
  But piping into 'play' will only work within system call.
-}
playV4, playV5 :: Orchestra.Output out => 
   FilePath -> (Name, Orchestra.T out, Score.T) -> IO ()

playV4 dir (name, o@(Orchestra.Cons (rate, _) _), s) =
   playNamedPipe ("csound32 -d -o stdout -s ", " 2>/dev/null" ++
                  " | play -t sw -r " ++ show rate ++
                         " -c " ++ show (Orchestra.channelCount o) ++
                         " - &")
                 dir name o s

playV5 dir (name, o, s) =
   playNamedPipe ("csound5 -d -o dac -s ", " >/dev/null &") dir name o s

playNamedPipe :: Orchestra.Output out =>
   (String, String) -> FilePath -> Name -> Orchestra.T out -> Score.T -> IO ()
playNamedPipe cmd dir name o s =
   let orchName  = dir ++ "/" ++ name ++ ".orc"
   in  do
          exists <- doesFileExist scorePipe
          if exists
            then putStrLn (scorePipe ++ " already exists")
            else void $ rawSystem "mkfifo" [scorePipe]
          writeFile orchName  (Orchestra.toString o)
          void $ system (fst cmd ++ orchName ++ " " ++ scorePipe ++ snd cmd)
          -- how can I reliably wait for CSound to open the pipe?
          writeFile scorePipe (Score.toString s)
          removeFile scorePipe


-- * Trials with shell-haskell that failed for reasons I don't know.

playV5NamedPipe :: Orchestra.Output out =>
   FilePath -> (Name, Score.T, Orchestra.T out) -> IO ()
playV5NamedPipe dir (name, s, o) =
   let orchName  = dir ++ "/" ++ name ++ ".orc"
       -- ((rate, _, channels), _) = o
   in  do
          catchCtrlC
--          system ("mkfifo "++scorePipe)
          writeFile orchName  (Orchestra.toString o)
--           system ("csound -d -odac "++orchName++" "++scorePipe++" &")
          (input,_,_) <- launch "csound"
             ["csound", "-d", "-odac", orchName, scorePipe]
--          IO.hPutStr input (Score.toString s)
          IO.hClose input
          writeFile scorePipe (Score.toString s)
--           removeFile scorePipe
--           outStr <- IO.hGetContents output
--           putStr outStr
--           IO.hClose output
          return ()

playV4NamedPipe :: Orchestra.Output out =>
   FilePath -> (Name, Score.T, Orchestra.T out) -> IO ()
playV4NamedPipe dir (name, s, o) =
   let orchName  = dir ++ "/" ++ name ++ ".orc"
       Orchestra.Cons (rate, _) _ = o
   in  do
          catchCtrlC
          writeFile orchName  (Orchestra.toString o)
          (input,_,_) <- launch "play"
              ["play", "-r", show rate,
                       "-c", show (Orchestra.channelCount o),
                       "-"]
          (_,_,output) <- launch "csound32"
              ["csound32", "-d", "-o", tmpWave, "-s",
                           orchName, scorePipe]
          writeFile scorePipe (Score.toString s)
          IO.hGetContents output >>= IO.hPutStr input
          IO.hClose input
          IO.hClose output

playV4AnonymousPipe :: Orchestra.Output out =>
   FilePath -> (Name, Score.T, Orchestra.T out) -> IO ()
playV4AnonymousPipe dir (name, s, o) =
   let orchName  = dir ++ "/" ++ name ++ ".orc"
       -- ((rate, _, channels), _) = o
   in  do
          catchCtrlC
          writeFile orchName  (Orchestra.toString o)
          (input,_,_) <- launch "csound32"
              ["csound32", "-d", "-o", tmpWave, "-W",
                           "-L", "stdin", orchName]
          IO.hPutStr input (Score.toString s)
          IO.hClose input
