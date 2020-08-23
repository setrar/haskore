% from AutoTrack by Stefan Ratschan

\begin{haskelllisting}

> module Haskore.Interface.AutoTrack.ScaleChart(T(Cons)) where

> import qualified Haskore.Basic.Scale  as Scale

\end{haskelllisting}


A certain type of event chart is a scale chart.

\begin{haskelllisting}

> newtype T = Cons Scale.T

\end{haskelllisting}

Conversion from chord chart to ScaleChart. This needs to be improved into a
sophisticated scale analyzer.

\begin{haskelllisting}

  fromChord :: (EventChart.T ChordSym) -> T
  fromChord (EventChart.C c) =
     let f d ch = (d, chordToScale ch)
     in  Cons (map f c)

\end{haskelllisting}
