% from AutoTrack by Stefan Ratschan

\subsection{Class of transposeable objects}

\begin{haskelllisting}

> module Haskore.Interface.AutoTrack.Transposeable(C, transpose) where

> import qualified Haskore.Basic.Pitch  as Pitch

\end{haskelllisting}

\subsection{Haskore Additions}

Here we turn to some stuff that really belongs into the Haskore core. First
transposition of pitch classes:

\begin{haskelllisting}

> class C a where
>   transpose :: Int -> a -> a

> instance C Pitch.Class where
>   transpose i pc = snd (Pitch.fromInt (Pitch.classToInt pc + i))

\end{haskelllisting}
