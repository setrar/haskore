\subsubsection{Dynamics}
\seclabel{dynamics}

\begin{haskelllisting}

> module Haskore.Basic.Dynamics where

\end{haskelllisting}

These definitions contradict to the rest of Haskore
where the normal Velocity is 1
and the default player makes crescendo relative to the starting velocity.
According the MIDI specification the velocity shall be a logarithmic scale,
thus it should be additive,
thus the normal velocity is 0.

\begin{haskelllisting}

> type Velocity = Rational
> type T = Rational

> normal, mp, p, pp, ppp, mf, f, ff, fff,
>    -- levels of softness
>    mezzoPiano, piano, pianissimo, pianoPianissimo,
>    -- levels of loudness
>    mezzoForte, forte, fortissimo, forteFortissimo :: Velocity

> normal = 0

> mezzoPiano = -1
> piano = -3
> pianissimo = -5
> pianoPianissimo = -7

> mezzoForte = 1
> forte = 3
> fortissimo = 5
> forteFortissimo = 7

> mp  = mezzoPiano
> p   = piano
> pp  = pianissimo
> ppp = pianoPianissimo

> mf  = mezzoForte
> f   = forte
> ff  = fortissimo
> fff = forteFortissimo

\end{haskelllisting}

Cf. MIDI 1.0 Detailed Specification, Document Version 4.2, February 1996,
page 10
