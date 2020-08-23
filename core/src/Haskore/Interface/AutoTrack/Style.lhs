% from AutoTrack by Stefan Ratschan

\section{Styles}

\begin{haskelllisting}

> module Haskore.Interface.AutoTrack.Style
>           (T, playToStream, jazz, bossa, takeFive, rock,
>            thomasCarib, harmonic) where

> import Data.Bool.HT (select, )
> import Data.List.HT (viewR, )
> import Haskore.Basic.Duration (en, qn, (%+), )
> import Haskore.Music ((+:+), (=:=), )

> import qualified Haskore.Composition.Rhythm as Rhythm
> import qualified Haskore.Composition.Drum   as Drum
> import qualified Haskore.Basic.Duration as Dur
> import qualified Haskore.Basic.Pitch    as Pitch
> import qualified Haskore.Music          as Music
> import qualified Haskore.Music.GeneralMIDI as MidiMusic
> import qualified Haskore.Melody         as Melody
> import qualified Haskore.Interface.MIDI.Render as MidiRender
> import qualified Sound.MIDI.File.Save   as MidiSave

> import qualified Haskore.Interface.AutoTrack.Transposeable as Transposeable
> import qualified Haskore.Interface.AutoTrack.ChordSymbol as ChordSymbol
> import qualified Haskore.Interface.AutoTrack.ChartBar    as ChartBar
> import qualified Haskore.Interface.AutoTrack.ChordChart  as ChordChart
> import qualified Haskore.Interface.AutoTrack.EventChart  as EventChart
> import qualified Haskore.Interface.AutoTrack.Instrument  as Instrument

> import qualified Data.ByteString.Lazy as B

\end{haskelllisting}

A style takes a chord chart and creates some music out of it.

\begin{haskelllisting}

> type T       = ChordChart.T -> MidiMusic.T
> type TMelody = ChordChart.T -> Melody.T ()

\end{haskelllisting}


\subsection{Filtering music}

Filtering certain parts from music,
in order to introduce rests \emph{after} the creation of some music.
The needed information can be encoded in several ways:

\begin{enumerate}
\item [ (Music.Dur, Music.Dur) ]: Place of rest, length of rest, sorted
\item [ Music.Dur ]: Place to switch from rest to music, or other way round
\item Music.Dur [ Bool ]: Some basic duration and then True implies music, False implies Rest
\end{enumerate}

We use the third possibility here, but use a helper function with a more general
interface, which additionally specifies the length of the first list member
(different from the basic duration).

\begin{haskelllisting}

> filterMusic :: Music.Dur -> [ Bool ] -> Music.T note -> Music.T note
> filterMusic = fm 0

> fm :: Music.Dur -> Music.Dur -> [ Bool ] -> Music.T note -> Music.T note
> fm fDur bDur plc =
>    Music.switchBinary
>       (\dur at -> case at of
>           (Just  _) -> Music.atom (min dur (musicDur fDur bDur plc)) at
>           (Nothing) -> Music.rest dur)
>       (\ctrl m -> case ctrl of
>           (Music.Tempo t) -> Music.changeTempo t (fm (fDur*t) (bDur*t) plc m)
>           _               -> Music.control ctrl m)
>       (\m0 m1 -> let m0' = fm fDur  bDur plc  m0
>                      (rFDur, rPlc) = remLen bDur plc (Music.dur m0 - fDur)
>                      m1' = fm rFDur bDur rPlc m1
>                  in m0' +:+ m1')
>       (\m0 m1 -> fm fDur bDur plc m0 =:= fm fDur bDur plc m1)
>       (Music.rest 0)

> remLen :: Music.Dur -> [ Bool ] -> Music.Dur -> (Music.Dur, [ Bool ])
> remLen bDur plc len =
>    if bDur>len
>    then (bDur-len, plc)
>    else (len-bDur, tail plc)

> musicDur :: (Num a) => a -> a -> [Bool] -> a
> musicDur fDur bDir plc =
>    sum (zipWith const (fDur : repeat bDir) (takeWhile id plc))
> --   sum (map fst (takeWhile snd (zip (fDur : repeat bDir) plc)))

\end{haskelllisting}


\subsection{Playing Styles}

Playing a chord chart and style into a stream of binary MIDI data.
We abuse a String to store it.

\begin{haskelllisting}

> playToStream :: Int -> T -> Integer -> Int -> ChordChart.T -> B.ByteString
> playToStream trans style tempo chornum chart =
>     let countin = Rhythm.countIn (ChartBar.dur (head (ChordChart.bars chart)))
>         choruses = Music.replicate chornum (style (Transposeable.transpose trans chart))
>         music = Music.changeTempo (tempo%+60) (countin +:+ choruses)
>     in MidiSave.toByteString (MidiRender.generalMidiDeflt music)

\end{haskelllisting}

\subsection{Drum Fill}

\begin{haskelllisting}

> jazzFill :: Music.Dur -> MidiMusic.T
> jazzFill d =
>    if d >= 2%+4
>      then
>        let shuffle dr =
>               Rhythm.toShuffledMusicWithDrumUnit en dr . Rhythm.fromString
>        in  Music.rest (d-2%+4) +:+
>               (shuffle Drum.SplashCymbal     "...x" =:=
>                shuffle Drum.AcousticBassDrum "...x" =:=
>                shuffle Drum.AcousticSnare    ".xx.")
>      else error "jazzFill: d must be at least 2%+4"

> endFill :: [ ChartBar.T ] -> MidiMusic.T
> endFill l =
>    let Just (initLd,lastLd) = viewR $ map ChartBar.dur l
>    in  Music.line (map Music.rest initLd) +:+
>        jazzFill lastLd

\end{haskelllisting}

\subsection{Bass Lines}

First some auxiliary function to play the bass note of a chord.

\begin{haskelllisting}

> bassFromMelody :: Melody.T () -> MidiMusic.T
> bassFromMelody =
>    MidiMusic.fromMelodyNullAttr MidiMusic.AcousticBass

> bassChoose :: (Music.Dur, ChordSymbol.T) -> Melody.T ()
> bassChoose (l, (ChordSymbol.Cons _ b _)) = bassNote l b

> bassNote :: Music.Dur -> Pitch.Class -> Melody.T ()
> bassNote l b =
>    Melody.note (Instrument.bottomRange Instrument.bass b) l ()

\end{haskelllisting}

\subsubsection{Chart Bass}

This bass line style plays the root of a chord on every chord of a chord chart.

\begin{haskelllisting}

> evFromCC :: ChordChart.T -> [(Music.Dur, ChordSymbol.T)]
> evFromCC = EventChart.events . EventChart.fromChordChart

> chartBass :: TMelody
> chartBass =
>    Music.line . map bassChoose . evFromCC

\end{haskelllisting}

\subsubsection{Quarter Bass}

This bass line style plays the root of the current chord on every quarter note.
It first creates chords on every beat, then maps bassChoose to it.
Problem: Right now only works if all chords are on quarter notes!

\begin{haskelllisting}

> splitToDur :: Music.Dur -> [ ( Music.Dur, e ) ] -> [ ( Music.Dur, e ) ]
> splitToDur sd =
>    concatMap (\(d,e) -> replicate (fromInteger (Dur.divide d sd)) (sd, e))

> quarterBass :: TMelody
> quarterBass =
>    Music.line . map bassChoose . splitToDur (1%+4) . evFromCC

> eighthBass :: TMelody
> eighthBass =
>    Music.line . map bassChoose . splitToDur (1%+8) . evFromCC

\end{haskelllisting}

\subsubsection{Bossa Bass}

A simple bass for Bossas using the bass note and its fifth.

\begin{haskelllisting}

> bossaBass :: TMelody
> bossaBass = Music.line . map bossaBassC . evFromCC

> bossaBassC :: (Music.Dur, ChordSymbol.T) -> Melody.T ()
> bossaBassC (l, ch@(ChordSymbol.Cons r _ _)) =
>   let r7 = Transposeable.transpose 7 r
>       bossa' = bassNote (3%+8) r +:+ bassNote (1%+8) r7 +:+
>                bassNote (1%+2) r7 +:+
>                bossaBassC (l - 1%+1, ch)
>   in select (bassChoose (l, ch))
>         [(l >= 1%+1, bossa'),
>          (l >= 1%+2, bassNote (3%+8) r +:+ bassNote (1%+8) r)]

\end{haskelllisting}

\subsubsection{Walking Bass Line}

Creating a good walking bass is a science in itself. There are numerous books which give
various rules for creating good bass lines. The following code is still VERY experimental
and just follows these basic rules:

\begin{itemize}
\item Create the root on the first quarter note of a chord, and
\item create random quarter notes of the appropriate scale for the rest.
\end{itemize}

We do this by creating a walking bass line for every chord of a chart separately
and then concatenating the created bass lines.

\begin{haskelllisting}

walking :: T
walking = Music.line . map walkChord . evFromCC c

\end{haskelllisting}

Walking bass line for a single chord of a certain length. Take the root for the
first note and random notes for the rest.

\begin{haskelllisting}

walkChord :: (Music.Dur, ChordSymbol.T) -> Melody.T ()
walkChord (d, ch) | (divisible d (1%+4)) =
             bassChoose ((1%+4), ch) +:+ walkRandom ((divide d (1%+4))-1) ch

\end{haskelllisting}

Create a random walking bass line of n quarter notes using chord ch.

\begin{haskelllisting}

walkRandom :: Int -> ChordSymbol.T -> Melody.T ()
walkRandom n ch = let scale = (chordToScale ch)
                      choice = \n -> bassChooseR n (1%+4, scale)
                  in  line (map choice (take n (randList (length scale))))

bassChooseR :: Int -> (Music.Dur, Scale) -> Melody.T ()
bassChooseR n (d, s) = Melody.note d (pitch (s!!n)) ()

\end{haskelllisting}

\subsection{Full Styles}

The jazz style works for 3/4 and 4/4 measure. It currently does not yet use walking bass,
but uses the quarter bass style above.

\begin{haskelllisting}

> jazzDrum :: Music.Dur -> MidiMusic.T
> jazzDrum d =
>    select (error "jazzDrum supports only 3%+4 and 4%+4")
>       [(d==3%+4, Rhythm.jazzWaltzRideP Drum.RideCymbal2 =:=
>                  Rhythm.jazzWaltzHiHatP Drum.PedalHiHat),
>        (d==4%+4, Music.replicate 2
>                    (Rhythm.jazzRideP Drum.RideCymbal2 =:=
>                     Rhythm.backBeatP Drum.PedalHiHat))]

> jazz :: T
> jazz s = let drums = Music.line (map (jazzDrum . ChartBar.dur) (ChordChart.bars s)) =:=
>                      endFill (ChordChart.bars s)
>              (bd, hc) = ChordChart.hasChord s
>          in  filterMusic bd hc drums =:=
>                 bassFromMelody (quarterBass s)
>

\end{haskelllisting}

The bossa style just plays the usual bossa clave with the hi-hat on the backbeat and some
simple bass.

\begin{haskelllisting}

> bossa :: T
> bossa c = let drums = Music.repeat Rhythm.claveBossa =:=
>                       Music.repeat Rhythm.ride =:=
>                       Music.repeat (Rhythm.backBeatP Drum.PedalHiHat)
>               bass = bassFromMelody (bossaBass c)
>           in Music.take ((4 * ChordChart.length c) %+ 4) drums =:= bass

\end{haskelllisting}

The Take-Five style works for charts with 5/4 measures only.

\begin{haskelllisting}

> takeFiveBass :: ChartBar.T -> Melody.T ()
> takeFiveBass b =
>    if ChartBar.dur b == 5%+4  &&  length (ChartBar.chords b) <= 2
>      then
>        let c=ChartBar.chords b
>            bass d Nothing  = Music.rest d
>            bass d (Just x) = bassChoose (d, x)
>        in if length c == 2
>             then bass (3%+4) (c!!0) +:+ bass (2%+4) (c!!1)
>             else bass (3%+4) (c!!0) +:+ bass (2%+4) (c!!0)
>      else error "takeFiveBass: only allowed for 5%+4 and maximally 2 chords per bar"

> takeFive :: T
> takeFive (ChordChart.Cons l) =
>     let rep pat = concat (replicate (length l) (Rhythm.fromString pat))
>         hiHatR  = rep "..x .x"
>         cymbalR = rep "x. xx x. x. xx"
>     in Rhythm.toMusicWithDrumUnit         qn Drum.PedalHiHat  hiHatR  =:=
>        Rhythm.toShuffledMusicWithDrumUnit en Drum.RideCymbal2 cymbalR =:=
>        endFill l =:=
>        bassFromMelody (Music.line (map takeFiveBass l))

\end{haskelllisting}

The rock style just plays the usual hi-hat eights, bass drum on downbeat, snare on backbeat.

\begin{haskelllisting}

> rock :: T
> rock c = let drums = Music.repeat Rhythm.basicBassDrum =:=
>                      Music.repeat Rhythm.basicSnare =:=
>                      Music.repeat Rhythm.basicHiHat
>              bass  = bassFromMelody (eighthBass c)
>          in Music.take ((4 * ChordChart.length c) %+ 4) drums =:= bass

\end{haskelllisting}

This style is not yet finished.

\begin{haskelllisting}

> thomasCarib :: T
> thomasCarib c =
>    Rhythm.backBeatP Drum.PedalHiHat =:=
>    Rhythm.basicBassDrum =:=
>    Rhythm.toShuffledMusicWithDrumUnit en Drum.Claves
>       (Rhythm.fromString ".. .x .x x.") =:=
>    bassFromMelody (chartBass c)

\end{haskelllisting}

This is a rather simple style
where the tones of a chord a played simultaneously.

\begin{haskelllisting}

> harmonic :: T
> harmonic =
>    let chordSymbolToMusic (dur, cs) = Music.chord $
>           map (\p -> Melody.note p dur ()) $
>              ChordSymbol.toChord cs
>    in  bassFromMelody . Music.line .
>           map chordSymbolToMusic . evFromCC

\end{haskelllisting}
