\subsection{Guitar}
\seclabel{guitar}

In this section we want to develop a simulation of a guitar.
This clearly demonstrates the power of our music-by-programming approach.
After writing some routines for doing the mechanical stuff
we can describe the music concisely as a sequence of chords.

\begin{haskelllisting}

> module Haskore.Example.Guitar where
>
> import qualified Haskore.Basic.Pitch as Pitch
> import           Haskore.Basic.Pitch (Class(..))
> import qualified Haskore.Basic.Duration as Dur
> -- import           Haskore.Melody.Standard   as StdMelody
> import           Haskore.Music.GeneralMIDI as MidiMusic
> import           Haskore.Music.Rhythmic    as RhyMusic
> import qualified Haskore.Melody            as Melody
> import qualified Haskore.Music             as Music
>
> import qualified Data.List as List

\end{haskelllisting}

% import qualified Haskore.Performance.Fancy as FancyPerformance


On a guitar a chord is not played
as an immediate sequence of the constituting notes,
but the order and the number of occurences of each tone
is adapted to the guitar and the possibilities of the player.
We want to automatically design a sequence of tones
that represents a given chord.
Our approach is simple:
For every string we choose the lowest possible note
which occurs in the chord.
This way we may miss notes of the chord,
but we have a good approximation.
If a chord consists of more than six notes,
we have to ignore some notes definitely.

For given pitches of all guitar strings
and the pitch classes of a chord,
\function{mapChordToString}
compute the tones that are played on each string of the guitar.

\begin{haskelllisting}

> mapChordToString :: [Pitch.T] -> [Pitch.Class] -> [Pitch.T]
> mapChordToString strs chrd =
>    map (choosePitchForString chrd) strs
>
> choosePitchForString :: [Pitch.Class] -> Pitch.T -> Pitch.T
> choosePitchForString chrd str@(_,pc) =
>    let diff x = mod (Pitch.classToInt x - Pitch.classToInt pc) 12
>        smallestDiff = minimum (map diff chrd)
>    in  Pitch.transpose smallestDiff str
>
> stringPitches :: [Pitch.T]
> stringPitches =
>    reverse [(-2,E), (-2,A), (-1,D), (-1,G), (-1,B), (0,E)]

\end{haskelllisting}

Once we obtain the tones that are played on a guitar
we want to arrange them into a guitar like melody.
We distinguish between up strokes and down strokes,
which are often played alternatingly.
According to the stroke direction,
the low notes are played slightly before the high ones
and vice versa.
We define the respective delays for each string.
Since both direction are perceived differently,
we have to prefetch the down strokes a bit.

\begin{haskelllisting}

> data Direction =
>      Up
>    | Down
>
> delayTime :: Dur
> delayTime = en/15
>
> dirDelays :: Direction -> [Dur.Offset]
> dirDelays dir =
>    map (Dur.toRatio delayTime *)
>       (case dir of
>          Up   -> [0..5]
>          Down -> [2,1..(-3)])

\end{haskelllisting}

Here is the only creative part:
The essential description of the guitar music.

\begin{haskelllisting}

> type UpDownPattern = [(Dur, Direction)]
>
> udp, udpInter, udpLast :: UpDownPattern
> udp      = [(qn,Up), (en,Down), (qn,Up), (en,Down), (qn,   Up)]
> udpInter = [(qn,Up), (en,Down), (qn,Up), (en,Down), (en,Up), (en,Down)]
> udpLast  = [(qn,Up), (en,Down), (qn,Up), (en,Down), (qn+wn,Up)]
>
> chords :: [([Pitch.Class], UpDownPattern)]
> chords =
>    [([C,E,G],    udp),
>     ([C,E,G,Bf], udp),
>     ([F,A,C],    udp),
>     ([F,Af,C],   udpInter),
>     ([C,E,G],    udp),
>     ([G,B,D],    udp),
>     ([C,F,G],    udp),
>     ([C,E,G],    udpLast)]

\end{haskelllisting}

The next step is to arrange the notes corresponding to the chords.

\begin{haskelllisting}

> type DelayedNote = (Dur.Offset, (Dur, Maybe Pitch.T))
>
> chordToPattern :: [Pitch.Class] -> UpDownPattern -> [[DelayedNote]]
> chordToPattern chrd =
>    map (\(dur,ord) ->
>       zipWith
>          (\delay p -> (delay, (dur, Just p)))
>          (dirDelays ord)
>          (mapChordToString stringPitches chrd))
>
> guitarEvents :: [[DelayedNote]]
> guitarEvents =
>    concatMap (uncurry chordToPattern) chords

\end{haskelllisting}

We want to simulate the guitar by a parallel composition of six strings.
The sound of each string finishes when the next sound on the string is played.
Thus we have to compute the time each string oscillates.
Finally we want to obtain this pattern of events:

\begin{verbatim}

   o             o  o
    o           o    o
     o         o      o
      o       o        o
       o     o          o
        o   o            o

\end{verbatim}

\begin{haskelllisting}

> delayNotes :: [DelayedNote] -> [Melody.T ()]
> delayNotes m =
>    let zero = (0, (0, Nothing))
>    in  zipWith
>           (\(d0, (dur, at)) (d1, _) ->
>                Music.atom (Dur.add (d1-d0) dur)
>                   (fmap (Melody.Note ()) at))
>           (zero : m) (m ++ [zero])
>
> stringMelodies :: [Melody.T ()]
> stringMelodies =
>    map (line . delayNotes) (List.transpose guitarEvents)
>
> parallelSong :: [instr] -> RhyMusic.T drum instr
> parallelSong instrs =
>    changeTempo 2 (chord (zipWith RhyMusic.fromMelodyNullAttr
>                                  instrs stringMelodies))
>
> parallelSongMIDI :: MidiMusic.T
> parallelSongMIDI =
>    transpose 12 (parallelSong (repeat MidiMusic.ElectricGuitarClean))

\end{haskelllisting}

Unfortunately the Guitar music appears to be slightly longer
than it is on the note sheet.
To workaround that we use notes of very short duration but very long legato.
For simplicity this simulation is not as precise as the one above.
We don't prefetch the down strokes and
we do not exactly care for the correct length of the string sounds.
The resulting MIDI files does still not sound satisfyingly
because notes of equal pitch overlap, which is not properly supported by MIDI.

\begin{verbatim}
<----------------->
               <-------------->
\end{verbatim}

The end of the first note terminates the second one, which is not intended.
Of course, you can play the MidiMusic using other back ends.

\begin{haskelllisting}

> chordWithLegatoPattern ::
>    [RhyMusic.T drum instr] -> UpDownPattern -> RhyMusic.T drum instr
> chordWithLegatoPattern tones pattern =
>    let beat (dur, dir) =
>           legato dur
>              (line (case dir of {Up -> tones; Down -> reverse tones}) +:+
>               Music.rest (dur - delayTime * List.genericLength tones))
>    in  line (map beat pattern)
>
>
>
> legatoSong :: [instr] -> RhyMusic.T drum instr
> legatoSong instrs =
>    changeTempo 2 (line (map
>       (uncurry
>          (chordWithLegatoPattern .
>           zipWith RhyMusic.fromMelodyNullAttr instrs .
>           map (Music.atom delayTime . Just . Melody.Note ()) .
>           mapChordToString stringPitches))
>       chords))
>
> legatoSongMIDI :: MidiMusic.T
> legatoSongMIDI =
>    transpose 12 (legatoSong (repeat MidiMusic.ElectricGuitarClean))

\end{haskelllisting}

% let strings = map (RhyMusic.fromStdMelody MidiMusic.ElectricGuitarClean) [a 0 delayTime [], b 0 delayTime [], c 0 delayTime []]
% chordWithLegatoPattern strings udp
% FancyPerformance.floatFromMusic (chordWithLegatoPattern strings udp)
