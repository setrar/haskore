\begin{haskelllisting}

> module Haskore.Melody where

> import Haskore.Basic.Pitch hiding (T)

> import qualified Haskore.Basic.Pitch    as Pitch
> import qualified Haskore.Basic.Duration as Duration
> import qualified Haskore.Music as Music
> import Data.Tuple.HT (mapSnd, )

> import qualified Medium

> import qualified Data.List as List
> import           Data.Maybe(fromMaybe)

> import qualified Data.Accessor.Basic      as Accessor

> data Note attr = Note {noteAttrs_ :: attr, notePitch_ :: Pitch.T}
>     deriving (Show, Eq, Ord)

> type T attr = Music.T (Note attr)

> noteAttrs :: Accessor.T (Note attr) attr
> noteAttrs =
>    Accessor.fromSetGet (\x n -> n{noteAttrs_ = x}) noteAttrs_
>
> notePitch :: Accessor.T (Note attr) Pitch.T
> notePitch =
>    Accessor.fromSetGet (\x n -> n{notePitch_ = x}) notePitch_

> toMelodyNullAttr :: T attr -> T ()
> toMelodyNullAttr =
>    Music.mapNote (\(Note _ p) -> Note () p)

\end{haskelllisting}

For convenience,
let's create simple names for familiar notes (\figref{note-names}),
durations, and rests (\figref{durations-rests}).
Despite the large number of them, these names are sufficiently
``unusual'' that name clashes are unlikely.

\begin{figure}{\small
\begin{haskelllisting}

> note :: Pitch.T -> Duration.T -> attr -> T attr
> note p d' nas = Medium.prim (Music.Atom d' (Just (Note nas p)))
>
> note' :: Pitch.Class -> Pitch.Octave ->
>            Duration.T -> attr -> T attr
> note' = flip (curry note)
>
> cf,c,cs,df,d,ds,ef,e,es,ff,f,fs,gf,g,gs,af,a,as,bf,b,bs ::
>    Pitch.Octave -> Duration.T -> attr -> T attr
>
> cf = note' Cf;  c = note' C;  cs = note' Cs
> df = note' Df;  d = note' D;  ds = note' Ds
> ef = note' Ef;  e = note' E;  es = note' Es
> ff = note' Ff;  f = note' F;  fs = note' Fs
> gf = note' Gf;  g = note' G;  gs = note' Gs
> af = note' Af;  a = note' A;  as = note' As
> bf = note' Bf;  b = note' B;  bs = note' Bs

\end{haskelllisting}
}
\caption{Convenient note construction functions.}
\figlabel{note-names}
\end{figure}

\begin{comment}

> {-
> o0,  o1, o2, o3, o4, o5, o6, o7, o8, o9,
>  s0, s1, s2, s3, s4, s5, s6, s7, s8, s9 ::
>       (Octave -> Duration.T -> attr -> T note)
>    ->           (Duration.T -> attr -> T note)
> o0 n = n 0; s0 n = n (- 1)
> o1 n = n 1; s1 n = n (- 2)
> o2 n = n 2; s2 n = n (- 3)
> o3 n = n 3; s3 n = n (- 4)
> o4 n = n 4; s4 n = n (- 5)
> o5 n = n 5; s5 n = n (- 6)
> o6 n = n 6; s6 n = n (- 7)
> o7 n = n 7; s7 n = n (- 8)
> o8 n = n 8; s8 n = n (- 9)
> o9 n = n 9; s9 n = n (-10)
> -}

\end{comment}

From the notes in the C major triad in register 4, I can now construct
a C major arpeggio and chord as well:
\begin{haskelllisting}

> cMaj :: [T ()]
> cMaj = map (\n -> n 4 Duration.qn ()) [c,e,g]  -- octave 4, quarter notes
>
> cMajArp, cMajChd :: T ()
> cMajArp = Music.line  cMaj
> cMajChd = Music.chord cMaj

\end{haskelllisting}

It is also possible to retrieve the pitch from a melody note.
But this should be avoided, since it must be dynamically checked,
whether the Melody value actually contains one note.

\begin{haskelllisting}

> noteToPitch :: T attr -> Pitch.T
> noteToPitch =
>    let err = error "leastVaryingInversions: melody must consist of a note"
>    in  Accessor.get notePitch .
>           Music.switchList (const (fromMaybe err)) err err err

\end{haskelllisting}


\paragraph*{Inversion and Retrograde.}

The notions of inversion, retrograde, retrograde inversion, etc. used
in 12-tone theory are also easily captured in Haskore.  First let's
define a transformation from a line created by \code{line} to a list:
\begin{haskelllisting}

> invertNote :: Pitch.T -> Note attr -> Note attr
> invertNote r =
>    Accessor.modify notePitch
>       (\ p -> Pitch.fromInt (2 * Pitch.toInt r - Pitch.toInt p))
>
> retro, invert, retroInvert, invertRetro ::
>    [(d, Music.Atom (Note attr))] -> [(d, Music.Atom (Note attr))]
> retro    = List.reverse
> invert l = let r = maybe
>                       (error "invert: first atom must be a note")
>                       (Accessor.get notePitch)
>                       (snd (head l))
>            in  map (mapSnd (fmap (invertNote r))) l
> retroInvert = retro  . invert
> invertRetro = invert . retro

\end{haskelllisting}

\begin{exercise} Show that ``\code{retro\ .\ retro}'',
``\code{invert\ .\ invert}'', and ``\code{retroInvert\ .\ invertRetro}''
are the identity on values created by \code{line}.
\end{exercise}

