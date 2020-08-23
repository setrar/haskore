% from AutoTrack by Stefan Ratschan

\section{Chord-Symbol-, Scale- and other Charts}
\label{sec:charts}

\begin{haskelllisting}

> module Haskore.Interface.AutoTrack.ChartBar
>           (T(Cons), dur, chords, readChordSymbol,
>            length) where

> import qualified Haskore.Music        as Music
> import qualified Data.List            as List
> import qualified Haskore.Interface.AutoTrack.ChordSymbol as ChordSymbol
> import qualified Haskore.Interface.AutoTrack.Transposeable as Transposeable
> import           Data.Char(isSpace, isAlpha)

> import           Haskore.Basic.Duration(wn, (%+), )

> import Prelude hiding (length)

\end{haskelllisting}

A bar consists of a time signature and a list of chord symbols. Bars have the following
input syntax:

\begin{verbatim}
  bar = { chord | timeSig | '%' | '_' }
  timeSig = '(' int '/' int ')'
\end{verbatim}

If no time signature is provided then a default is used (within chord chart the time
signature of the bar before, and 4/4 for the first bar). The character '\%' is a short-cut
for the chords just before. The character '\_' denotes a break. 

\begin{haskelllisting}

> data T = Cons {
>     dur    :: Music.Dur,
>     chords :: [ Maybe ChordSymbol.T ]
>   } deriving Show
>
> length :: Integral a => T -> a
> length = fromIntegral . List.length . chords

> instance Read T where
>   readsPrec _ = readChordSymbol wn Nothing

> readChordSymbol :: Music.Dur -> Maybe ChordSymbol.T -> ReadS T
> readChordSymbol oldSig oldChord (c:s) | isSpace c = readChordSymbol oldSig oldChord s
> readChordSymbol _      oldChord s@('(':_) = 
>     [ (Cons r b, r2) | (r, r1) <- readSig s, 
>                            (Cons _ b, r2) <- readChordSymbol r oldChord r1 ]
> readChordSymbol oldSig (Just chord) ('%':r) = 
>     [ (Cons oldSig (Just chord:b), r1) | (Cons _ b, r1) <- readChordSymbol oldSig (Just chord) r ]
> readChordSymbol oldSig (Just _) ('_':r) = 
>     [ (Cons oldSig (Nothing:b), r1) | (Cons _ b, r1) <- readChordSymbol oldSig Nothing r ]
> readChordSymbol oldSig _ s@(d:_) | isAlpha d = 
>     [ (Cons oldSig (Just c:b), r2) | (c, r1) <- reads s, 
>                                     (Cons _ b, r2) <- readChordSymbol oldSig (Just c) r1 ] 
> readChordSymbol oldSig _ s = [ (Cons oldSig [], s) ]
>
> readSig :: ReadS Music.Dur
> readSig s@('(':_) =
>    let readRatio s' =
>           [ (p%+q, r1) |
>                 (p,'/':r) <- reads s', (q, r1) <- reads r ]
>    in  readParen True readRatio s
> readSig _ = []

\end{haskelllisting}

Bars can be transposed.

\begin{haskelllisting}

> instance Transposeable.C T where
>   transpose i (Cons d l) = Cons d (fmap (fmap (Transposeable.transpose i)) l)

\end{haskelllisting}
