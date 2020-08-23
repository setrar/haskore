% from AutoTrack by Stefan Ratschan

\section{Rhythm}

\begin{haskelllisting}

> module Haskore.Composition.Rhythm where
>
> import qualified Haskore.Composition.Drum  as Drum
> import qualified Haskore.Basic.Duration    as Dur
> import qualified Haskore.Music             as Music
> import qualified Haskore.Music.GeneralMIDI as MidiMusic
> import qualified Haskore.Music.Rhythmic    as RhyMusic
> import Haskore.Basic.Duration (qn, en, sn, (%+), )
> import Data.List.HT (mapAdjacent, )
> import Data.Bool.HT (select, )
> import Data.Char (isSpace, )

\end{haskelllisting}

There are many different possibilities for dealing with the notion of rhythm.
Some of them are:

\begin{itemize}
\item Modeling it as a succession of notes and rests of equal length
\item Allowing notes and rests to be of different (integer or rational) lengths
\item Dealing with rhythm on the level of the \texttt{RhyMusic.T} data type, without any
  special data type for modeling rhythm
\end{itemize}

We will use the first possibility here. The third possibility has been used in
Martin Schwenke's \texttt{DrumMachine} module.

As explained above we think of rhythm as a succession of notes and rests of
equal length. For this we use lists of booleans, where \texttt{True} means that
a note is played, and \texttt{False} means that no note is played (i.e. a
rest).

\begin{haskelllisting}

> type T = [ Bool ]

\end{haskelllisting}

By default the basic rhythmical unit is one sixteenth note. The \texttt{Rhythm.T}
data-type does not depend on this, it only comes into the game when we convert
rhythms to music.

\begin{haskelllisting}

> unit :: Music.Dur
> unit = sn

\end{haskelllisting}

We provide two ways of creating rhythms:

\begin{itemize}
\item From strings, where an 'x' means that some note is played at this place, and any
  other character means that no note is played, while white spaces are ignored.
\item From ordered lists of integers, where every integer means that at the place with
  this number we have a note (the first place is zero).  On all the other places we have
  rests.
\end{itemize}

\begin{haskelllisting}

> fromString :: String -> T
> fromString = map ('x'==) . filter (not . isSpace)

> fromPositions :: [ Int ] -> T
> fromPositions l =
>    let hitAfter x = replicate (x-1) False ++ [ True ]
>        checkPos d =
>           if d>0
>             then d
>             else error ("fromPositions: list of time events must increase strictly monotonously")
>    in  concatMap hitAfter (mapAdjacent ((checkPos .) . subtract) ((-1):l))

\end{haskelllisting}

Now we want to convert rhythms to music.
We do this using two data types,
which one can immediately convert to music via function application.

\begin{haskelllisting}

> type ToMusicWithMusic drum instr = RhyMusic.T drum instr -> T -> RhyMusic.T drum instr
> type ToMusicWithDrum  drum instr = drum -> T -> RhyMusic.T drum instr

> toMusicWithMusic :: ToMusicWithMusic drum instr
> toMusicWithMusic m r =
>    let play b = if b then m else Music.rest (Music.dur m)
>    in  Music.line (map play r)

> toMusicWithDrum :: ToMusicWithDrum drum instr
> toMusicWithDrum = toMusicWithDrumUnit unit

\end{haskelllisting}

Sometimes we also want to specify a basic rhythmical unit which is different from the
default one.

\begin{haskelllisting}

> toMusicWithDrumUnit :: Music.Dur -> ToMusicWithDrum drum instr
> toMusicWithDrumUnit d p = toMusicWithMusic (Drum.toMusic p d Drum.na)

\end{haskelllisting}

Finally one can also create shuffled music from rhythms.

\begin{haskelllisting}

> toShuffledMusicWithDrum :: ToMusicWithDrum drum instr
> toShuffledMusicWithDrum = toShuffledMusicWithDrumUnit unit

> toShuffledMusicWithDrumUnit :: Music.Dur -> ToMusicWithDrum drum instr
> toShuffledMusicWithDrumUnit d p r =
>    let stretch = 1%+3
>        dstr    = Dur.scale (1+stretch) d
>        dcompr  = Dur.scale (1-stretch) d
>        play b  =
>           if b
>             then flip (Drum.toMusic p) Drum.na
>             else Music.rest
>    in  Music.line (zipWith play r (cycle [dstr, dcompr]))

\end{haskelllisting}

Some basic rhythms:

\begin{haskelllisting}

> tickR, downBeatR, backBeatR, claveR, claveRumbaR,
>   claveBossaR, clave5, clave7, jazzRideR,
>   jazzWaltzRideR, jazzWaltzHiHatR :: T

> tickR = fromString "x"

> downBeatR = fromString "x."
> backBeatR = fromString ".x"

> claveR = fromString "x..x..x.  ..x.x..."

> claveRumbaR = fromString "x..x...x  ..x.x..."

> claveBossaR = fromString "x..x..x.  ..x..x.."

> clave5 = fromString "..x.x"

> clave7 = fromString ".x.x..x"

> jazzRideR = fromString "x.xx"

> jazzWaltzRideR = fromString "x.xxx."
> jazzWaltzHiHatR = fromString "..x"

> countInR :: Music.Dur -> T
> countInR d =
>    select (error "countIn not defined for this measure")
>       [(d == 4%+4, fromString "x.x.xxxx"),
>        (d == 5%+4, fromString "x..x.xxxxx"),
>        (Dur.divisible d qn,
>           let b = fromInteger (Dur.divide d qn)
>           in  True : replicate (b-1) False ++ replicate b True)]

\end{haskelllisting}

In one more step in the conversion to music we fix the basic rhythmical unit and shuffle/straight.

\begin{haskelllisting}

> tickP, claveP, claveRumbaP, claveBossaP, jazzRideP,
>   jazzWaltzRideP, jazzWaltzHiHatP, downBeatP,
>   backBeatP :: drum -> RhyMusic.T drum instr

> tickP       = flip (toMusicWithDrumUnit en) tickR
> claveP      = flip (toMusicWithDrumUnit en) claveR
> claveRumbaP = flip (toMusicWithDrumUnit en) claveRumbaR
> claveBossaP = flip (toMusicWithDrumUnit en) claveBossaR

> jazzRideP = flip (toShuffledMusicWithDrumUnit en) jazzRideR

> jazzWaltzRideP  = flip (toShuffledMusicWithDrumUnit en) jazzWaltzRideR
> jazzWaltzHiHatP = flip (toMusicWithDrumUnit qn) jazzWaltzHiHatR

> downBeatP = flip (toMusicWithDrumUnit qn) downBeatR
> backBeatP = flip (toMusicWithDrumUnit qn) backBeatR

\end{haskelllisting}

And now we assign these rhythms to instruments.

\begin{haskelllisting}

> click, clave, claveRumba, claveBossa, metro5, metro7,
>   basicBassDrum, basicSnare, basicHiHat, ride :: MidiMusic.T

> click = Music.repeat (tickP Drum.Claves)

> clave      = claveP      Drum.Claves
> claveRumba = claveRumbaP Drum.Claves
> claveBossa = claveBossaP Drum.Claves

> metro5 = toMusicWithDrumUnit qn Drum.Claves (cycle clave5)
> metro7 = toMusicWithDrumUnit qn Drum.Claves (cycle clave7)

> basicBassDrum = downBeatP Drum.AcousticBassDrum
> basicSnare    = backBeatP Drum.AcousticSnare
> basicHiHat    = tickP     Drum.ClosedHiHat
> ride          = tickP     Drum.RideCymbal2

> countIn :: Music.Dur -> MidiMusic.T
> countIn m = toMusicWithDrumUnit qn Drum.Claves (countInR m)

\end{haskelllisting}
