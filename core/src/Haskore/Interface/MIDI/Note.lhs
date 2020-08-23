
A MIDI note is an interim data structure
which shall be stored in a \type{Performance.BackEnd.T} list of events.
It stores each note as a single record,
that is it is not split into note-on and note-off.

\begin{haskelllisting}

> module Haskore.Interface.MIDI.Note where

> import qualified Haskore.Interface.MIDI.InstrumentMap as InstrMap
> import qualified Sound.MIDI.General                   as GeneralMidi
> import qualified Sound.MIDI.Message.Channel           as ChannelMsg
> import qualified Sound.MIDI.Message.Channel.Voice     as Voice
> import qualified Haskore.Music.GeneralMIDI            as MidiMusic
> import qualified Haskore.Music.Rhythmic               as RhyMusic
> import qualified Haskore.Basic.Pitch                  as Pitch
> import           Data.Ord.HT (limit, )
> import           Data.Maybe.HT (toMaybe, )

> data T =
>    Cons {
>      velocityOn  :: ChannelMsg.Velocity,
>      velocityOff :: ChannelMsg.Velocity,
>      channel     :: ChannelMsg.Channel,
>      program     :: ChannelMsg.Program,
>      pitch       :: ChannelMsg.Pitch
>    }

\end{haskelllisting}

You can convert a MidiNote from and to a pair of MIDI events.
This is used in \module{MIDI.Read} and \module{MIDI.Write}, respectively.
\begin{haskelllisting}

> fromMIDIEvents :: (ChannelMsg.T, ChannelMsg.T) -> Maybe T
> fromMIDIEvents
>    (ChannelMsg.Cons c0 (ChannelMsg.Voice (Voice.NoteOn  p0 v0)),
>     ChannelMsg.Cons c1 (ChannelMsg.Voice (Voice.NoteOff p1 v1))) =
>       let progErr = error ("program depends on channel settings - " ++
>                            "still not determined")
>       in  toMaybe (c0 == c1 && p0 == p1)
>                   (Cons v0 v1 c0 progErr p0)
> fromMIDIEvents _ = Nothing

> toMIDIEvents :: T -> (ChannelMsg.T, ChannelMsg.T)
> toMIDIEvents note =
>    let chan = channel     note
>        p    = pitch       note
>        vOn  = velocityOn  note
>        vOff = velocityOff note
>        me0 = ChannelMsg.Cons chan (ChannelMsg.Voice (Voice.NoteOn  p vOn))
>        me1 = ChannelMsg.Cons chan (ChannelMsg.Voice (Voice.NoteOff p vOff))
>    in  (me0, me1)

\end{haskelllisting}

A MidiNote can be constructed from several kinds of notes.
Here are two instances for notes of generic rhythmic music
and General MIDI notes.
These converters are also the functions
where the maps from instrument types to MIDI programs go into.
The first set of functions is need for writing MIDI files.
\begin{haskelllisting}

> fromRhyNote :: RealFrac dyn =>
>    InstrMap.ToChannelProgramPitch drum ->
>    InstrMap.ToChannelProgram instr ->
>       dyn -> Pitch.Relative -> RhyMusic.Note drum instr -> T
> fromRhyNote dMap iMap dyn trans (RhyMusic.Note vel body) =
>    let velMidi = velocityFromStd dyn vel
>    in  case body of
>           RhyMusic.Tone instr p ->
>              let (chan, prog) = iMap instr
>              in  Cons velMidi velMidi
>                       chan prog (pitchFromStd trans p)
>           RhyMusic.Drum drum ->
>              let (chan, prog, key) = dMap drum
>              in  Cons velMidi velMidi chan prog key

> fromGMNote :: RealFrac dyn =>
>    InstrMap.ToChannel MidiMusic.Instr ->
>       dyn -> Pitch.Relative -> MidiMusic.Note -> T
> fromGMNote iMap =
>    fromRhyNote
>       (\drum  -> (GeneralMidi.drumChannel,
>                   GeneralMidi.drumProgram,
>                   GeneralMidi.drumToKey drum))
>       (\instr -> (iMap instr, Voice.toProgram (fromEnum instr)))

> velocityFromStd :: RealFrac dyn =>
>    dyn -> Rational -> Voice.Velocity
> velocityFromStd dyn vel =
>    Voice.toVelocity $
>    round (limit (0, fromIntegral (Voice.fromVelocity Voice.maximumVelocity))
>                 (dyn * fromRational vel *
>                  fromIntegral (Voice.fromVelocity Voice.normalVelocity)))

> pitchFromStd :: Pitch.Relative -> Pitch.T -> Voice.Pitch
> pitchFromStd trans p =
>    -- MIDI pitch is in range because of range checks on Pitch construction
>    Voice.increasePitch (Pitch.toInt p + trans) Voice.zeroKey

\end{haskelllisting}

The second set of functions is need for reading MIDI files.
\begin{haskelllisting}

> toRhyNote ::
>    InstrMap.FromChannelProgramPitch drum ->
>    InstrMap.FromChannelProgram instr ->
>    T -> RhyMusic.Note drum instr
> toRhyNote dMap iMap (Cons v _ ch prog mp) =
>    let drum  = dMap (ch, prog, mp)
>        instr = iMap (ch, prog)
>    in  RhyMusic.Note (velocityToStd v)
>           (case (drum,instr) of
>              (Nothing, Nothing) ->
>                  error "MidiNote.toRhyNote: channel+program not found"
>              (Just _, Just _) ->
>                  error "MidiNote.toRhyNote: note can be drum or instrument"
>              (Just drum', Nothing) ->
>                  RhyMusic.Drum drum'
>              (Nothing, Just instr') ->
>                  RhyMusic.Tone instr' (pitchToStd mp))

> toGMNote :: T -> MidiMusic.Note
> toGMNote =
>    toRhyNote
>       (\(ch, _, mp) ->
>            toMaybe (ch==GeneralMidi.drumChannel)
>                    (GeneralMidi.drumFromKey mp))
>       (\(ch, prog) ->
>            toMaybe (ch/=GeneralMidi.drumChannel)
>                    (GeneralMidi.instrumentFromProgram prog))

\end{haskelllisting}

Load the velocity.
This shouldn't be mixed up with the volume.
The volume which is controlled by the MIDI Volume controller
simply scales the signal
whereas the velocity is an instrument specific value
that corresponds to the intensity with which the instrument is played.

\begin{haskelllisting}

> velocityToStd :: Fractional a => Voice.Velocity -> a
> velocityToStd x =
>    fromIntegral (Voice.fromVelocity x) /
>    fromIntegral (Voice.fromVelocity Voice.normalVelocity)

> pitchToStd :: Voice.Pitch -> Pitch.T
> pitchToStd p = Pitch.fromInt (Voice.subtractPitch Voice.zeroKey p)

\end{haskelllisting}
