\subsubsection{Trills}

\begin{haskelllisting}

> module Haskore.Composition.Trill where
>
> import qualified Haskore.Music as Music

\end{haskelllisting}

A \keyword{trill} is an ornament that alternates rapidly between two (usually
adjacent) pitches.  Let's implement a trill as a function that take a note as
an argument and returns a series of notes whose durations add up to the same
duration as as the given note.

A trill alternates between the given note and another note, usually the note
above it in the scale.  Therefore, it must know what other note to use.  So
that the structure of \function{trill} remains parallel across different keys, we'll
implement the other note in terms of its interval from the given note in half
steps.  Usually, the note is either a half-step above (interval = 1) or a
whole-step above (interval = 2).  Using negative numbers, a trill that goes to
lower notes can even be implemented.

Also, the trill needs to know how fast to alternate between the two notes.
One way is simply to specify the type of smaller note to use.
(Another implementation will be discussed later.)
So, our \function{trill} has the following type:
\begin{haskelllisting}

> trill :: Int -> Music.Dur -> Music.T note -> Music.T note

\end{haskelllisting}
Its implementation:
\begin{haskelllisting}

> trill i d m =
>    let atom = Music.take d m
>    in  Music.line (Music.takeLine (Music.dur m)
>           (cycle [atom, Music.transpose i atom]))

\end{haskelllisting}
Since the function uses \function{Music.tranpose}
one can even trill more complex objects like chords.

The next version of \function{trill} starts on the second note,
rather than the given note.
It is simple to define a function that starts on the other note:
\begin{haskelllisting}

> trill' :: Int -> Music.Dur -> Music.T note -> Music.T note
> trill' i sDur m =
>       trill (negate i) sDur (Music.transpose i m)

\end{haskelllisting}
Another way to define a trill is in terms of the number of subdivided notes
to be included in the trill.
\begin{haskelllisting}

> trillN :: Int -> Integer -> Music.T note -> Music.T note
> trillN i nTimes m =
>       trill i (Music.dur m / fromIntegral nTimes) m

\end{haskelllisting}
This, too, can be made to start on the other note.
\begin{haskelllisting}

> trillN' :: Int -> Integer -> Music.T note -> Music.T note
> trillN' i nTimes m =
>       trillN (negate i) nTimes (Music.transpose i m)

\end{haskelllisting}

Finally, a \function{roll} can be implemented as a trill whose interval is
zero.  This feature is particularly useful for percussion.
\begin{haskelllisting}

> roll  :: Music.Dur -> Music.T note -> Music.T note
> rollN :: Integer   -> Music.T note -> Music.T note
>
> roll  d      = trill  0 d
> rollN nTimes = trillN 0 nTimes

\end{haskelllisting}
