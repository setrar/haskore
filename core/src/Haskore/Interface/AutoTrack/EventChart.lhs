% from AutoTrack by Stefan Ratschan

\begin{haskelllisting}

> module Haskore.Interface.AutoTrack.EventChart
>    (T(Cons), events, fromChordChart, fromChartBar) where

> import qualified Haskore.Music        as Music
> import qualified Haskore.Interface.AutoTrack.ChartBar    as ChartBar
> import qualified Haskore.Interface.AutoTrack.ChordChart  as ChordChart
> import qualified Haskore.Interface.AutoTrack.ChordSymbol as ChordSymbol
> import qualified Haskore.Interface.AutoTrack.Transposeable as Transposeable
> import qualified Haskore.Basic.Duration as Dur
> import qualified Data.List as List
> import           Data.Maybe(fromJust)

\end{haskelllisting}

Event charts are currently not used. An event chart represents a list of objects of a
certain type and duration (the ``events'').

\begin{haskelllisting}

> data T e = Cons {events :: [ (Music.Dur, e) ] } deriving Show

> fromChordChart :: ChordChart.T -> T ChordSymbol.T
> fromChordChart (ChordChart.Cons c) =
>    Cons (concatMap (events . fromChartBar) c)

> fromChartBar :: ChartBar.T -> T ChordSymbol.T
> fromChartBar (ChartBar.Cons d l) =
>    let f c = (d / Dur.fromRatio (List.genericLength l), fromJust c)
>    in  Cons (map f l)

\end{haskelllisting}

Transpose an event chart by a certain number of semitones

\begin{haskelllisting}

> instance (Transposeable.C a) => Transposeable.C (T a) where
>   transpose i = fmap (Transposeable.transpose i)

\end{haskelllisting}

Event charts can also act as functors:

\begin{haskelllisting}

> instance Functor T where
>   fmap f (Cons v) = Cons (map ( \(d, c) -> (d, f c) ) v)

\end{haskelllisting}
