% from AutoTrack by Stefan Ratschan

\begin{haskelllisting}

> module Haskore.Interface.AutoTrack.ChordChart
>           (T(Cons), bars, hasChord,
>            length, concat) where

> import qualified Haskore.Music        as Music
> import qualified Haskore.Interface.AutoTrack.ChartBar as ChartBar
> import qualified Haskore.Interface.AutoTrack.Transposeable as Transposeable
> import qualified Haskore.Basic.Duration as Dur
> import           Haskore.Basic.Duration (wn, (%+), )
> import           Data.Char(isSpace)
> import qualified Data.List as List

> import Prelude hiding (length, concat)

\end{haskelllisting}


Chord charts are lists of bars. They have the following input syntax:

\begin{verbatim}
  chart = { (bar | '%') '|' }
\end{verbatim}

The character '\%' is a shortcut for the same bar as before.
Comments can occur everywhere in the text.
They start with "--" and continue till the end of the current line.

\begin{haskelllisting}

> data T = Cons {bars :: [ ChartBar.T ] } deriving Show

> length :: Integral a => T -> a
> length = fromIntegral . List.length . bars

> instance Read T where
>   readsPrec _ = Haskore.Interface.AutoTrack.ChordChart.read

> concat :: T -> T -> T 
> concat (Cons x) (Cons y) = Cons (x++y)

> read :: ReadS T
> read s = read1 (ChartBar.Cons wn []) (filterComment s)

> filterComment :: String -> String
> filterComment ('-':'-':r) = filterComment (tail (snd (break (=='\n') r)))
> filterComment (c:r) = (c:filterComment r)
> filterComment "" = ""

> read1 :: ChartBar.T -> ReadS T
> read1 lb (c:r) | isSpace c = 
>     read1 lb (dropWhile isSpace r)
> read1 lb ('%':r) = 
>     [ (Cons (lb:br), r2) |
>         ('|':r1) <- [ dropWhile isSpace r ],
>         (Cons br, r2) <- read1 lb r1 ]
> read1 (ChartBar.Cons sig _) s@(_:_) =
>     [ (Cons (b:br), r1) |
>         (b, ('|':r)) <- ChartBar.readChordSymbol sig Nothing s,
>         (Cons br, r1) <- read1 b r ]
> read1 _ s = [ (Cons [], s) ]

\end{haskelllisting}

Chord charts can be transposed.

\begin{haskelllisting}

> instance Transposeable.C T where
>   transpose i (Cons c) = Cons (fmap (Transposeable.transpose i) c)

\end{haskelllisting}

We can extract a Boolean list from a chord chart that tells whether there is a chord at a certain position
(hc[i] is true iff d*i has a chord).

\begin{haskelllisting}

> hasChord :: T -> (Music.Dur, [ Bool ] )
> hasChord c =
>    let g = barGCD c
>    in (g, hasChord1 g c)

> hasChord1 :: Music.Dur -> T -> [ Bool ]
> hasChord1 bDur (Cons c) = List.concat (map (hasChordBar bDur) c)

> barUnit :: ChartBar.T -> Music.Dur -> Dur.Ratio
> barUnit bar d = d * (1 %+ ChartBar.length bar)

> hasChordBar :: Music.Dur -> ChartBar.T -> [ Bool ]
> hasChordBar bDur bar@(ChartBar.Cons d chords) =
>    let times =
>           fromInteger
>              (Dur.divide (barUnit bar d) bDur)
>        createList = replicate times . maybe False (const True)
>    in  concatMap createList chords

> barGCD :: T -> Music.Dur
> barGCD (Cons c) =
>    let chordDur bar = barUnit bar (ChartBar.dur bar)
>    in  foldr1 Dur.gcd (map chordDur c)

\end{haskelllisting}
