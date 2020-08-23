\subsubsection{Instrument map}
\seclabel{user-patch-map}

\begin{haskelllisting}

> module Haskore.Interface.MIDI.InstrumentMap where

> import Haskore.Music.Standard(Instr)
> import qualified Sound.MIDI.Message.Channel as ChannelMsg
> import qualified Sound.MIDI.General         as GeneralMidi

> import qualified Haskore.General.Map as Map
> import qualified Data.List as List
> import Data.Tuple.HT (swap, )
> import Data.Char (toLower, )
> import Data.Maybe (fromMaybe, )

\end{haskelllisting}

A \type{InstrumentMap.ChannelProgramTable} is a user-supplied table for mapping instrument
names (\type{Instr}s) to Midi channels and General Midi patch names.
The patch names are by default General Midi names, although the user
can also provide a \type{PatchMap} for mapping Patch Names to
unconventional Midi Program Change numbers.
\begin{haskelllisting}

> type ChannelTable instr =
>    [(instr,  ChannelMsg.Channel)]
> type ChannelProgramTable instr =
>    [(instr, (ChannelMsg.Channel, ChannelMsg.Program))]
> type ChannelProgramPitchTable instr =
>    [(instr, (ChannelMsg.Channel, ChannelMsg.Program, ChannelMsg.Pitch))]
>
> type ToChannel instr =
>    instr ->  ChannelMsg.Channel
> type ToChannelProgram instr =
>    instr -> (ChannelMsg.Channel, ChannelMsg.Program)
> type ToChannelProgramPitch instr =
>    instr -> (ChannelMsg.Channel, ChannelMsg.Program, ChannelMsg.Pitch)
>
> type FromChannel instr =
>     ChannelMsg.Channel -> Maybe instr
> type FromChannelProgram instr =
>    (ChannelMsg.Channel, ChannelMsg.Program) -> Maybe instr
> type FromChannelProgramPitch instr =
>    (ChannelMsg.Channel, ChannelMsg.Program, ChannelMsg.Pitch) -> Maybe instr

\end{haskelllisting}

The \function{allValid} is used to test whether or not every instrument
in a list is found in a \type{InstrumentMap.ChannelProgramTable}.

\begin{haskelllisting}

> repair :: [Instr] -> ChannelProgramTable Instr -> ChannelProgramTable Instr
> repair insts pMap =
>    if allValid pMap insts
>        then pMap
>        else tableFromInstruments insts
>
> allValid :: ChannelProgramTable Instr -> [Instr] -> Bool
> allValid upm = all (\x -> any (partialMatch x . fst) upm)

\end{haskelllisting}

If a Haskore user only uses General Midi instrument names as
\type{Instr}s, we can define a function that automatically creates a
\type{InstrumentMap.ChannelProgramTable} from these names.  Note that, since there are only 15
Midi channels plus percussion, we can handle only 15 instruments.
Perhaps in the future a function could be written to test whether or
not two tracks can be combined with a Program Change (tracks can be
combined if they don't overlap).
\begin{haskelllisting}

> tableFromInstruments :: [Instr] -> ChannelProgramTable Instr
> tableFromInstruments instrs =
>    zip instrs (assignChannels GeneralMidi.instrumentChannels instrs)
>         -- 10th channel (#9) is for percussion

> assignChannels :: [ChannelMsg.Channel] -> [Instr] ->
>       [(ChannelMsg.Channel, ChannelMsg.Program)]
> assignChannels _ [] = []
> assignChannels [] _ =
>    error "Too many instruments; not enough MIDI channels."
> assignChannels chans@(c:cs) (i:is) =
>    let percList = ["percussion", "perc", "drum", "drums"]
>    in  if map toLower i `elem` percList
>          then (GeneralMidi.drumChannel, GeneralMidi.drumProgram)
>                      : assignChannels chans is
>          else (c, fromMaybe
>             (error ("unknown instrument <<" ++ i ++ ">>"))
>             (GeneralMidi.instrumentNameToProgram i))
>                      : assignChannels cs is

> fromInstruments :: Ord instr => [instr] -> ToChannel instr
> fromInstruments instrs =
>    let fm = Map.fromList (zip instrs GeneralMidi.instrumentChannels)
>    in  Map.findWithDefault fm (error "More instruments than channels")

\end{haskelllisting}

The following functions lookup \type{Instr}s in \type{InstrumentMap.ChannelProgramTable}s to
recover channel and program change numbers.
Note that the function that does string matching ignores case,
and that instrument name and search pattern match
if one is a prefix of the other one.
For example, \code{"chur"} matches \code{"Church Organ"}.  Note also
that the {\em first} match succeeds, so using a substring should be
done with care to be sure that the correct instrument is selected.
\begin{haskelllisting}

> partialMatch :: Instr -> Instr -> Bool
> partialMatch "piano" "Acoustic Grand Piano" = True
> partialMatch s1 s2 =
>   let s1' = map toLower s1
>       s2' = map toLower s2
>   in  all (uncurry (==)) (zip s1' s2')
>
> lookupIName :: [(Instr, a)] -> Instr -> a
> lookupIName  ys x =
>    maybe (error ("InstrumentMap.lookupIName: Instrument " ++ x ++ " unknown"))
>          snd (List.find (partialMatch x . fst) ys)
>
> lookup :: Eq instr => [(instr, a)] -> instr -> a
> lookup ys x =
>    fromMaybe (error ("InstrumentMap.lookup: Instrument unknown"))
>              (List.lookup x ys)

\end{haskelllisting}

\begin{haskelllisting}

> reverseLookupMaybe :: Eq a => [(instr, a)] -> a -> Maybe instr
> reverseLookupMaybe ys x =
>    List.lookup x (map swap ys)

> reverseLookup :: Eq a => [(instr, a)] -> a -> instr
> reverseLookup ys x =
>    let instr = reverseLookupMaybe ys x
>        err   = error "InstrumentMap.reverseLookup: channel+program not found"
>    in  fromMaybe err instr

\end{haskelllisting}

A default \type{InstrumentMap.ChannelProgramTable}.
Note: the PC sound card I'm using is limited to 9 instruments.

\begin{haskelllisting}

> defltTable :: [(Instr, ChannelMsg.Channel, GeneralMidi.Instrument)]
> defltTable =
>    map (\(instr,chan,gmInstr) -> (instr, ChannelMsg.toChannel chan, gmInstr))
>    [("piano",   1, GeneralMidi.AcousticGrandPiano),
>     ("vibes",   2, GeneralMidi.Vibraphone),
>     ("bass",    3, GeneralMidi.AcousticBass),
>     ("flute",   4, GeneralMidi.Flute),
>     ("sax",     5, GeneralMidi.TenorSax),
>     ("guitar",  6, GeneralMidi.AcousticGuitarSteel),
>     ("violin",  7, GeneralMidi.Viola),
>     ("violins", 8, GeneralMidi.StringEnsemble1),
>     ("drums",   9, GeneralMidi.AcousticGrandPiano)]
>       -- the GM name for drums is unimportant, only channel 9

> deflt :: ChannelProgramTable Instr
> deflt =
>    map (\(iName, chan, gmName) ->
>          (iName, (chan, GeneralMidi.instrumentToProgram gmName))) defltTable

> defltGM :: ChannelProgramTable GeneralMidi.Instrument
> defltGM =
>    map (\(_, chan, gmName) ->
>          (gmName, (chan, GeneralMidi.instrumentToProgram gmName))) defltTable

> defltCMap :: [(GeneralMidi.Instrument, ChannelMsg.Channel)]
> defltCMap =
>    map (\(_, chan, gmName) -> (gmName, chan)) defltTable

\end{haskelllisting}
