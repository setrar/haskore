\subsection{MML}

\begin{haskelllisting}

> module Haskore.Interface.MML where

> import qualified Haskore.Basic.Pitch    as Pitch
> import qualified Haskore.Music          as Music
> import qualified Haskore.Melody         as Melody
> import           Haskore.Basic.Duration((%+))

> import Control.Monad.Trans.State (State, state, evalState, )

\end{haskelllisting}

I found some music notated in a language called MML.
The description consists of strings.

\begin{itemize}
\item
 \code{l}$n$ determines the duration of subsequent notes:
 \code{l1} - whole note,
 \code{l2} - half note,
 \code{l4} - quarter note and so on.
\item \code{>} switch to the octave above
\item \code{<} switch to the octave below
\item Lower case letter \code{a} - \code{g} play the note of the corresponding pitch class.
\item \code{\#} (sharp) or \code{-} (flat) may follow a note name
in order to increase or decrease, respectively, the pitch of the note by a semitone.
\item An additional figure for the note duration may follow.
\item \code{p} is pause.
\end{itemize}

See \module{Kantate147} for an example.

%\url{http://www.student.oulu.fi/~vtatila/history_of_game_music.html}

\begin{haskelllisting}

> type Accum = (Music.Dur, Pitch.Octave)

> barToMusic :: String -> Accum -> ([Melody.T ()], Accum)
> barToMusic []     accum      = ([], accum)
> barToMusic (c:cs) (dur, oct) =
>    let charToDur dc = 1 %+ read (dc:[])
>        prependAtom atom adur (ms, newAccum) =
>           (atom adur : ms, newAccum)
>        procNote ndur pitch c0s =
>           let mkNote c1s = prependAtom (flip (Melody.note (oct, pitch)) ())
>                                        ndur (barToMusic c1s (dur, oct))
>           in  case c0s of
>                 '#':c1s -> procNote ndur (succ pitch) c1s
>                 '-':c1s -> procNote ndur (pred pitch) c1s
>                 c1 :c1s -> if '0'<=c1 && c1<='9'
>                            then procNote (charToDur c1) pitch c1s
>                            else mkNote c0s
>                 []      -> mkNote c0s
>    in  case c of
>          'c' -> procNote dur Pitch.C cs
>          'd' -> procNote dur Pitch.D cs
>          'e' -> procNote dur Pitch.E cs
>          'f' -> procNote dur Pitch.F cs
>          'g' -> procNote dur Pitch.G cs
>          'a' -> procNote dur Pitch.A cs
>          'b' -> procNote dur Pitch.B cs
>          'p' -> let (c1:c1s) = cs
>                 in  prependAtom Music.rest (charToDur c1)
>                                 (barToMusic c1s (dur, oct))
>          '<' -> barToMusic cs (dur, oct-1)
>          '>' -> barToMusic cs (dur, oct+1)
>          'l' -> let (c1:c1s) = cs
>                 in  barToMusic c1s (charToDur c1, oct)
>          _   -> error ("unexpected character '"++[c]++"' in Haskore.Interface.MML description")

> toMusicState :: String -> State Accum [Melody.T ()]
> toMusicState s = state (barToMusic s)

> toMusic :: Pitch.Octave -> String -> Melody.T ()
> toMusic oct s = Music.line (evalState (toMusicState s) (0, oct))

\end{haskelllisting}
