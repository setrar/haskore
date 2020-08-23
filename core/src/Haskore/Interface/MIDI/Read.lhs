\subsubsection{Reading Midi files}
\seclabel{Haskore.Interface.MIDI.Read}

Now that we have translated a raw Midi file into a \code{MidiFile.T} data type,
we can translate that \code{MidiFile.T} into a \code{MidiMusic.T} object.

\begin{haskelllisting}

> module Haskore.Interface.MIDI.Read (toRhyMusic, toGMMusic,
>  {- debugging -} retrieveTracks)
>   where
>
> import qualified Haskore.Interface.MIDI.Note          as MidiNote
> import qualified Haskore.Interface.MIDI.InstrumentMap as InstrMap
> import           Sound.MIDI.File                  as MidiFile
> import qualified Sound.MIDI.File.Event            as MidiFileEvent
> import qualified Sound.MIDI.Message.Channel       as ChannelMsg
> import qualified Sound.MIDI.Message.Channel.Voice as Voice
> import qualified Sound.MIDI.General               as GeneralMidi
> import Sound.MIDI.File.Event (T(MIDIEvent, MetaEvent), )
> import Sound.MIDI.File.Event.Meta (T(SetTempo), defltTempo, )
> import Sound.MIDI.Message.Channel (Body(Voice), Channel, )
> import Sound.MIDI.Message.Channel.Voice (Program, )
>
> import Haskore.Basic.Duration ((%+))
> import qualified Data.EventList.Relative.TimeBody  as TimeList
> import qualified Data.EventList.Relative.MixedBody as TimeList
> import qualified Haskore.Music             as Music
> import qualified Haskore.Music.GeneralMIDI as MidiMusic
> import qualified Haskore.Music.Rhythmic    as RhyMusic
> import qualified Haskore.Performance.Context  as Context
> import qualified Haskore.Performance.BackEnd  as PfBE
> import qualified Haskore.Performance.Default  as DefltPf
> import qualified Haskore.Process.Optimization as Optimization

> import qualified Numeric.NonNegative.Class as NonNeg

> import Haskore.Music
>              (line, chord, changeTempo, Dur, DurRatio)
> import Data.Tuple.HT (mapPair, mapSnd, )
> import qualified Data.List.HT as ListHT
>
> import Haskore.General.Map (Map)
> import qualified Haskore.General.Map as Map
> import Data.Maybe (mapMaybe, fromMaybe)

\end{haskelllisting}

The main function.
Note that we need drum and instrument maps
in order to restore a \code{Context.T}
as well as a \code{RhyMusic.T} object.
\begin{haskelllisting}

> toRhyMusic ::
>    (NonNeg.C time, Fractional time, Real time, Fractional dyn) =>
>    InstrMap.ChannelProgramPitchTable drum ->
>    InstrMap.ChannelProgramTable instr ->
>    MidiFile.T ->
>       (Context.T time dyn (RhyMusic.Note drum instr), RhyMusic.T drum instr)
> toRhyMusic dMap iMap mf@(MidiFile.Cons _ d trks) =
>   let cpm = makeCPM trks
>       m   = Music.mapNote
>                (MidiNote.toRhyNote
>                   (InstrMap.reverseLookupMaybe dMap)
>                   (InstrMap.reverseLookupMaybe iMap))
>                (format (readFullTrack d cpm) (MidiFile.explicitNoteOff mf))
>   in (context, m)

> toGMMusic ::
>    (NonNeg.C time, Fractional time, Real time, Fractional dyn) =>
>    MidiFile.T -> (InstrMap.ChannelTable MidiMusic.Instr,
>                   Context.T time dyn MidiMusic.Note, MidiMusic.T)
> toGMMusic mf@(MidiFile.Cons _ d trks) =
>   let cpm     = makeCPM trks
>       upm     = map (\(ch, progNum) ->
>                       (GeneralMidi.instrumentFromProgram progNum, ch))
>                     (Map.toList cpm)
>       m       = Music.mapNote MidiNote.toGMNote
>                    (format (readFullTrack d cpm)
>                            (MidiFile.explicitNoteOff mf))
>   in (upm, context, m)

> context ::
>    (NonNeg.C time, Fractional time, Real time, Fractional dyn) =>
>    Context.T time dyn note
> context =
>    Context.setPlayer DefltPf.player $
>    Context.setDur 2 $
>    DefltPf.context

> retrieveTracks :: MidiFile.T -> [[MidiMusic.T]]
> retrieveTracks (MidiFile.Cons _ d trks) =
>   let cpm = makeCPM trks
>   in  map (map (Music.mapNote MidiNote.toGMNote
>                  . readTrack (MidiFile.ticksPerQuarterNote d) cpm . fst)
>              . prepareTrack) trks

> type ChannelProgramMap = Map ChannelMsg.Channel Voice.Program

> readFullTrack ::
>    Division -> ChannelProgramMap -> Track -> Music.T MidiNote.T
> readFullTrack dv cpm =
>   let readTempoTrack (t,r) =
>          changeTempo r (readTrack (MidiFile.ticksPerQuarterNote dv) cpm t)
>   in  Optimization.all . line . map readTempoTrack . prepareTrack

> prepareTrack :: Track -> [(RichTrack, DurRatio)]
> prepareTrack =
>    map (extractTempo defltTempo) . segmentBeforeSetTempo .
>    mergeNotes defltTempo . moveTempoToHead

\end{haskelllisting}

Make one big music out of the individual tracks of a MidiFile,
using different composition types depending on the format of the MidiFile.
\begin{haskelllisting}

> format :: (Track -> Music.T note) -> MidiFile.T -> Music.T note
> format tm (MidiFile.Cons typ _ trks) =
>    let trks' = map tm trks
>    in  case typ of
>           MidiFile.Mixed ->
>              case trks' of
>                 [trk] -> trk
>                 _ -> error ("toRhyMusic: Only one track allowed for MIDI file type 0.")
>           MidiFile.Parallel -> chord trks'
>           MidiFile.Serial   -> line  trks'

\end{haskelllisting}


Look for Program Changes in the given tracks,
in order to make a \code{ChannelProgramMap}.
\begin{haskelllisting}

> makeCPM :: [Track] -> ChannelProgramMap
> makeCPM =
>    Map.fromList . concatMap (mapMaybe getPC . TimeList.getBodies)
>
> getPC :: MidiFileEvent.T -> Maybe (Channel, Program)
> getPC ev =
>    do (ch, Voice.ProgramChange num) <- MidiFileEvent.maybeVoice ev
>       Just (ch, num)

\end{haskelllisting}

\code{moveTempoToHead} gets the information that occurs at the beginning of
the piece: the default tempo and the default key signature.
A \code{SetTempo} in the middle of the piece
should translate to a tempo change (\code{Tempo r m}),
but a \code{SetTempo} at time 0 should set the default
tempo for the entire piece, by translating to \code{Context.T} tempo.
It remains a matter of taste which tempo of several parallel tracks
to use for the whole music.
\code{moveTempoToHead} takes care of all events that occur at time 0
so that if any \code{SetTempo} appears at time 0,
it is moved to the front of the list,
so that it can be easily retrieved from the result of
\code{segmentBeforeSetTempo}.
\begin{haskelllisting}

> moveTempoToHead :: Track -> Track
> moveTempoToHead es =
>    let (tempo, track) = getHeadTempo es
>    in  TimeList.cons 0 (MetaEvent (SetTempo tempo)) track

> getHeadTempo :: Track -> (Tempo, Track)
> getHeadTempo es =
>    maybe
>       (defltTempo, es)
>       (\ ~(me,rest) ->
>           case me of
>              MetaEvent (SetTempo tempo) -> (tempo, rest)
>              _ -> mapSnd (TimeList.cons 0 me) (getHeadTempo rest))
>       (do ((0,me),rest) <- TimeList.viewL es
>           return (me,rest))

\end{haskelllisting}

Manages the tempo changes in the piece.
It translates each MidiFile \code{SetTempo}
into a ratio between the new tempo and the tempo at the beginning.
\begin{haskelllisting}

> extractTempo :: Tempo -> RichTrack -> (RichTrack, DurRatio)
> extractTempo d trk =
>    fromMaybe
>       (trk, 1)
>       (do ((_, Event (MetaEvent (SetTempo tempo))), rest) <- TimeList.viewL trk
>           return (rest, toInteger d %+ toInteger tempo))

\end{haskelllisting}

\code{segmentBefore} is used to split a track into sub-tracks by tempo.
We do not want to add this function to the \code{event-list} package,
because the precise type would be
\type{AlternatingList.Disparate (TimeList.T time body) (TimeList.Event time body)}
and that's inconvenient for our application.
\begin{haskelllisting}

> segmentBefore ::
>    (body -> Bool) -> TimeList.T time body -> [TimeList.T time body]
> segmentBefore p =
>    map TimeList.fromPairList .
>    ListHT.segmentBefore (p . snd) .
>    TimeList.toPairList

\end{haskelllisting}

\begin{haskelllisting}

> isSetTempo :: RichEvent -> Bool
> isSetTempo (Event (MetaEvent (SetTempo _))) = True
> isSetTempo _                                = False

> segmentBeforeSetTempo :: RichTrack -> [RichTrack]
> segmentBeforeSetTempo = segmentBefore isSetTempo

\end{haskelllisting}

\code{readTrack} is the heart of the \code{toRhyMusic} operation.
It reads a track that has been processed by \code{mergeNotes},
and returns the track as \code{StdMusic.T}.
A \code{RichEvent} consists either of a normal \code{MIDIEvent}
or of a note, which in contrast to normal \code{MIDIEvent}s
contains the information of corresponding \code{NoteOn} and \code{NoteOff} events.

\begin{haskelllisting}

> type RichTrack = TimeList.T ElapsedTime RichEvent
> data RichEvent =
>      Event MidiFileEvent.T
>    | Note  ElapsedTime MidiNote.T

> readTrack :: Tempo -> ChannelProgramMap ->
>    RichTrack -> Music.T MidiNote.T
> readTrack ticksPerQN cpm =
>    PfBE.toMusic . trackTimeToStd ticksPerQN
>     . richTrackToBE . applyProgChanges cpm

\end{haskelllisting}

Take the division in ticks per quarterNote and
a duration value in number of ticks and
converts that to a common note duration
(such as quarter note, eighth note, etc.).
\begin{haskelllisting}

> fromTicks :: Tempo -> ElapsedTime -> Dur
> fromTicks ticksPerQN d =
>    toInteger d %+ (toInteger ticksPerQN * quarter)

> quarter :: Integer
> quarter = 4

> trackTimeToStd :: Tempo ->
>    PfBE.T ElapsedTime note -> PfBE.T Dur note
> trackTimeToStd ticksPerQN =
>    TimeList.mapBody
>       (\(PfBE.Event d n) -> PfBE.Event (fromTicks ticksPerQN d) n)
>       . TimeList.mapTime (fromTicks ticksPerQN)

\end{haskelllisting}

Look up an instrument name from a \code{ChannelProgramMap} given its channel number.
\begin{haskelllisting}

> lookupChannelProg :: ChannelProgramMap -> Channel -> Program
> lookupChannelProg cpm =
>    Map.findWithDefault cpm
>       (error "Invalid channel in user patch map")

\end{haskelllisting}

Implement a \keyword{Program Change}: a change in the \code{ChannelProgramMap} in
which a channel changes from one instrument to another.
\begin{haskelllisting}

> progChange :: Channel -> Program -> ChannelProgramMap -> ChannelProgramMap
> progChange = Map.insert
> -- progChange ch num cpm = Map.insert ch num cpm

\end{haskelllisting}

Process all \code{ProgramChange} events in a track.
That is, manage a patch map and
insert in the appropriate program numbers into the \type{MidiNote.T}s.

The function works the following way:
Split the track into pieces, each beginning with a program change.
Compute the patch maps that are active after each program change.
Apply these patch maps to the track parts.
\begin{haskelllisting}

> isProgChange :: RichEvent -> Bool
> isProgChange (Event ev) =
>    maybe False (const True) (getPC ev)
> isProgChange _ = False

> applyProgChanges :: ChannelProgramMap -> RichTrack -> RichTrack
> applyProgChanges cpm track =
>    let parts@(_:pcParts) = segmentBefore isProgChange track
> {-
>        updateCPM (Event (MIDIEvent ch (ProgramChange prog))) =
>           progChange ch prog
>        updateCPM _  =  error "TimeList.collectCoincident is buggy"
> -}
>        updateCPM =
>           TimeList.switchL
>              (error "TimeList.collectCoincident is buggy")
>              (\ (_, Event ev) _ ->
>                  maybe
>                     (error "after segmentation, each part should start with ProgramChange event")
>                     (uncurry progChange)
>                     (getPC ev))
>        cpms =
>           scanl (flip id) cpm (map updateCPM pcParts)
>        setProg localCPM (Note d n) =
>           Note d (n{MidiNote.program =
>                        lookupChannelProg localCPM (MidiNote.channel n)})
>        setProg _ e = e
>    in  TimeList.concat (zipWith (TimeList.mapBody . setProg) cpms parts)

\end{haskelllisting}

Remove meta events from \type{RichTrack},
thus converting to a back-end performance.
\begin{haskelllisting}

> richNoteToBE :: RichEvent -> PfBE.Event ElapsedTime MidiNote.T
> richNoteToBE (Note d n) = PfBE.Event d n
> richNoteToBE _ = error "richNoteToBE: only Note constructor allowed"

> isRichNote :: RichEvent -> Bool
> isRichNote (Note _ _) = True
> isRichNote _          = False

> richTrackToBE :: RichTrack -> PfBE.T ElapsedTime MidiNote.T
> richTrackToBE =
>    TimeList.mapBody richNoteToBE . fst
>       . TimeList.partition isRichNote

\end{haskelllisting}

The \code{mergeNotes} function changes the order of the events in a track
so that they can be handled by readTrack: each \code{NoteOff}
is put directly after its corresponding \code{NoteOn}. Its first and second
arguments are the elapsed time and value (in microseconds per quarter
note) of the \code{SetTempo} currently in effect.
\begin{haskelllisting}

> mergeNotes :: Tempo -> Track -> RichTrack
> mergeNotes stv =
>    TimeList.mapTimeTail
>       (TimeList.switchBodyL $ \ e rest ->
>            uncurry TimeList.consBody $
>            let deflt = (Event e, mergeNotes stv rest)
>            in  case e of
>                   MetaEvent (SetTempo newStv) ->
>                      (Event e, mergeNotes newStv rest)
>                   MIDIEvent chmsg@(ChannelMsg.Cons _ (Voice msg)) ->
>                      if Voice.isNoteOn msg
>                        then mapPair
>                                (uncurry Note, mergeNotes stv)
>                                (searchNoteOff 0 stv 1 chmsg rest)
>                        else
>                          if Voice.isNoteOff msg
>                            then error "NoteOff before NoteOn"
>                            else deflt
>                   _ -> deflt)

\end{haskelllisting}

The function \code{searchNoteOff} takes a track and
looks through the list of events to find the \code{NoteOff}
corresponding to the given \code{NoteOn}.
A \code{NoteOff} corresponds to an earlier \code{NoteOn}
if it is the first in the track to have the same channel and pitch.
If between \code{NoteOn} and \code{NoteOff} are \code{SetTempo} events,
it calculates what the elapsed-time is,
expressed in the current tempo.
This function takes a ridiculous number of arguments,
I know, but I don't think it can do without any of the information.
Maybe there is a simpler way.
\begin{haskelllisting}

> searchNoteOff ::
>       Double          {- ^ time interval between NoteOn and now,
>                            in terms of the tempo at the NoteOn -}
>    -> Tempo -> Double {- ^ SetTempo values: the one at the NoteOn and
>                            the ratio between the current tempo and the first one. -}
>    -> ChannelMsg.T    {- ^ channel and pitch of NoteOn (NoteOff must match) -}
>    -> Track           {- ^ the track to be searched -}
>    -> ((ElapsedTime, MidiNote.T), Track)
>                       -- ^ the needed event and the remainder of the track
>
> searchNoteOff int ost str chm0 =
>    TimeList.switchL
>       (error "ReadMidi.searchNoteOff: no corresponding NoteOff")
>       (\(t1, mev1) es ->
>           maybe
>              -- if MIDI events don't match, then recourse
>              (mapSnd (TimeList.cons t1 mev1) $
>               searchNoteOff (addInterval str t1 int) ost
>                  (case mev1 of
>                     -- respect tempo changes
>                     MetaEvent (SetTempo nst) ->
>                          fromIntegral ost / fromIntegral nst
>                     _ -> str)
>                  chm0 es)
>              -- if MIDI events match, construct a MidiNote.T
>              (\note ->
>                 let d = round (addInterval str t1 int)
>                 in  ((d, note), TimeList.delay t1 es))
>              -- check whether NoteOn and NoteOff matches
>              (do chm1 <- MidiFileEvent.maybeMIDIEvent mev1
>                  MidiNote.fromMIDIEvents (chm0, chm1)))

> addInterval :: Double -> ElapsedTime -> Double -> Double
> addInterval str t int = int + fromIntegral t * str

\end{haskelllisting}
