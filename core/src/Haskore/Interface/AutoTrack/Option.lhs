% from AutoTrack by Stefan Ratschan

For extracting the options from the command line
we use the \texttt{GetOpt} package proviced by \texttt{ghc}.
This is currently a little bit of a mess.
It should be reimplemented using the technique described at
\url{http://www.haskell.org/haskellwiki/GetOpt}.

\begin{haskelllisting}

> module Option(T, getAll) where

> import qualified Haskore.Music.GeneralMIDI as MidiMusic
> import qualified Haskore.Interface.AutoTrack.Style      as Style
> import qualified Haskore.Interface.AutoTrack.ChordChart as ChordChart

> import System.Console.GetOpt (getOpt, usageInfo,
>            ArgDescr(NoArg, ReqArg), OptDescr(Option), ArgOrder(Permute))
> import System.Environment (getArgs)
> import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))

> import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
> import Data.List (intersperse)

> {-
> Should be a record with one constructor and multiple fields, i.e.
> data T = Cons {optError :: String, optTempo :: Integer, ...}
> This should replace Tuple.
> -}

> data T = Error     String
>        | Tempo     Integer
>        | Style     Style.T
>        | Transpose Int
>        | Choruses  Int
>        | Help

> isHelp :: T -> Bool
> isHelp Help = True
> isHelp _    = False

> errorToMaybe :: T -> Maybe String
> errorToMaybe (Option.Error m) = Just m
> errorToMaybe _                = Nothing

> -- should be [ OptDescr (T -> T) ]
> options :: [ OptDescr T ]
> options = [ Option [ 't' ] [ "tempo" ] (ReqArg tempoOption "TEMPO") "TEMPO of track",
>             Option [ 'r' ] [ "transpose" ] (ReqArg transposeOption "TRANSPOSE") "TRANSPOSE track",
>             Option [ 's' ] [ "style" ] (ReqArg styleOption "STYLE") "music STYLE",
>             Option [ 'c' ] [ "choruses" ] (ReqArg chorusesOption "CHORUSES") "number of CHORUSES",
>             Option [ 'h' ] [ "help" ] (NoArg Option.Help) "display usage" ]

> tempoOption, transposeOption, styleOption,
>   chorusesOption :: String -> T

> tempoOption     = Option.Tempo     . read
> transposeOption = Option.Transpose . read

> styles :: [(String, ChordChart.T -> MidiMusic.T)]
> styles = [("jazz",     Style.jazz),
>           ("bossa",    Style.bossa),
>           ("takeFive", Style.takeFive),
>           ("rock",     Style.rock),
>           ("harmonic", Style.harmonic)]

> styleOption s =
>    maybe (Option.Error ("Unknown style '"++s++"'\n"))
>          Option.Style (lookup s styles)

> chorusesOption = Choruses . read

> usage :: String
> usage = usageInfo "\nUsage: track [OPTION...] <infile >outfile\n" options ++
>        "\nAvailable styles: " ++ concat (intersperse ", " (map fst styles)) ++ "\n\n" ++
>        "This program is free software; you can redistribute it and/or\n" ++
>        "modify it under the terms of the GNU General Public License\n" ++
>        "as published by the Free Software Foundation; either version 2\n" ++
>        "of the License, or (at your option) any later version.\n\n" ++
>        "This program is distributed in the hope that it will be useful,\n" ++
>        "but WITHOUT ANY WARRANTY; without even the implied warranty of\n" ++
>        "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n" ++
>        "GNU General Public License for more details.\n"

> processAll :: [String] -> IO [T]
> processAll argv =
>         case (getOpt Permute options argv) of
>              (o,_,[]  ) -> return o
>              (_,_,errs) -> fail (concat errs ++ usage)

> getDefault :: (a -> Maybe b) -> [ a ] -> b -> b
> getDefault b t def = fromMaybe def $ listToMaybe $ mapMaybe b t

> getTempo :: T -> Maybe Integer
> getTempo (Option.Tempo t) = Just t
> getTempo _ = Nothing

> getTrans, getChoruses :: T -> Maybe Int
> getTrans (Option.Transpose t) = Just t
> getTrans _ = Nothing

> getChoruses (Option.Choruses c) = Just c
> getChoruses _ = Nothing

> getStyle :: T -> Maybe (ChordChart.T -> MidiMusic.T)
> getStyle (Option.Style s) = Just s
> getStyle _ = Nothing

> type Tuple = (Integer, Style.T, Int, Int)

> toTuple :: [ T ] -> Tuple
> toTuple l  = (getDefault getTempo l 120,
>               getDefault getStyle l Style.jazz,
>               getDefault getTrans l 0,
>               getDefault getChoruses l 5)

> exit :: Bool -> String -> IO a
> exit c m = do putStr (m ++ usage)
>               if c then exitWith ExitSuccess else exitWith (ExitFailure 1)

> getError :: [ T ] -> Maybe String
> getError = listToMaybe . mapMaybe errorToMaybe

> getAll :: IO Tuple
> getAll  = do opts <- (getArgs >>= processAll)
>              if any isHelp opts
>                then exit True ""
>                else maybe (return (toTuple opts)) (exit False) (getError opts)

\end{haskelllisting}
