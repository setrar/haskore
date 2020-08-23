% from AutoTrack by Stefan Ratschan

\section{Instruments}

\begin{haskelllisting}

> module Haskore.Interface.AutoTrack.Instrument
>           (T, bass, bottomRange, topRange) where

> import qualified Haskore.Basic.Pitch  as Pitch

\end{haskelllisting}

Here we store various information about instruments. Currently the only information is the
range of an instrument (its highest possible and lowest possible note).

\begin{haskelllisting}

> data T = Cons { lowest, highest :: Pitch.T }

> bass :: T
> bass = Cons { lowest=(2, Pitch.E), highest=(7, Pitch.G) }   -- ???

\end{haskelllisting}

Create the deepest/highest note of a certain pitchclass, that an instrument can create.

\begin{haskelllisting}

> bottomRange :: T -> Pitch.Class -> Pitch.T
> bottomRange instr cl =
>    let (boct, bcl) = lowest instr
>    in if cl > bcl
>         then (boct,   cl)
>         else (boct+1, cl)

> topRange :: T -> Pitch.Class -> Pitch.T
> topRange instr cl =
>    let (boct, bcl) = highest instr
>    in if cl < bcl
>         then (boct,   cl)
>         else (boct-1, cl)

\end{haskelllisting}
