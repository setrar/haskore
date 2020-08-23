\subsubsection{Pitch}
\seclabel{pitch}

Perhaps the most basic musical idea is that of a \keyword{pitch},
which consists of an \keyword{octave} and a \keyword{pitch class}
(i.e. one of 12 semi-tones, cf. \secref{discussion:pitch}):
\begin{haskelllisting}

> module Haskore.Basic.Pitch where

> import Data.Ix(Ix)

> type T      = (Octave, Class)
> data Class  = Cf | C | Cs | Df | D | Ds | Ef | E | Es | Ff | F | Fs
>             | Gf | G | Gs | Af | A | As | Bf | B | Bs
>      deriving (Eq,Ord,Ix,Enum,Show,Read)
> type Octave = Int

\end{haskelllisting}
So a \type{Pitch.T} is a pair consisting of a pitch class and an octave.
Octaves are just integers, but we define a datatype for pitch classes,
since distinguishing enharmonics (such as $G^\#$ and $A^b$) may be important
(especially for notation).
\figref{note-freqs} shows the meaning of the some \type{Pitch.T} values.

\begin{figure}
\begin{center}
\begin{tabular}{llr}
$A_2$ & \code{(-3,A)} &  27.5 Hz \\
$A_1$ & \code{(-2,A)} &  55.0 Hz \\
$A  $ & \code{(-1,A)} & 110.0 Hz \\
$a  $ & \code{( 0,A)} & 220.0 Hz \\
$a^1$ & \code{( 1,A)} & 440.0 Hz \\
$a^2$ & \code{( 2,A)} & 880.0 Hz
\end{tabular}
\end{center}
\caption{Note names, Haskore representations and frequencies.}
\figlabel{note-freqs}
\end{figure}

Treating pitches simply as integers is useful in many settings,
so let's also define some functions for converting between \type{Pitch.T}
values and \type{Pitch.Absolute} values (integers):
\begin{haskelllisting}

> type Absolute = Int
> type Relative = Int
>
> toInt :: T -> Absolute
> toInt (oct,pc) = 12*oct + classToInt pc
>
> fromInt :: Absolute -> T
> fromInt ap =
>    let (oct, n) = divMod ap 12
>    in  (oct, [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] !! n)
>
> classToInt :: Class -> Relative
> classToInt pc = case pc of
>      Cf -> -1;  C ->  0; Cs ->  1   -- or should Cf be 11?
>      Df ->  1;  D ->  2; Ds ->  3
>      Ef ->  3;  E ->  4; Es ->  5
>      Ff ->  4;  F ->  5; Fs ->  6
>      Gf ->  6;  G ->  7; Gs ->  8
>      Af ->  8;  A ->  9; As -> 10
>      Bf -> 10;  B -> 11; Bs -> 12   -- or should Bs be 0?

\end{haskelllisting}

Now two functions for parsing and formatting pitch classes
in a more human way, that is using '\#' and 'b' suffixes
instead of 's' and 'f'.
We do not simply use 

\begin{haskelllisting}

> classParse :: ReadS Class
> classParse (p:'#':r) = reads (p:'s':r)
> classParse (p:'b':r) = reads (p:'f':r)
> classParse r = reads r

> classFormat :: Class -> ShowS
> classFormat pc =
>    let (p:r) = show pc
>    in  (p:) .
>        case r of
>           [] -> id
>           's':[] -> ('#':)
>           'f':[] -> ('b':)
>           _ -> error ("classFormat: Pitch.Class.show must not return suffixes" ++
>                       " other than 's' and 'f'")

\end{haskelllisting}

Using \type{Pitch.Absolute} we can compute the frequency associated
with a pitch:

\begin{haskelllisting}

> intToFreq :: Floating a => Absolute -> a
> intToFreq ap = 440 * 2 ** (fromIntegral (ap - toInt (1,A)) / 12)

\end{haskelllisting}

We can also define a function \function{Pitch.transpose},
which transposes pitches
(analogous to \function{Music.transpose},
which transposes values of type \type{Music.T}):
\begin{haskelllisting}

> transpose :: Relative -> T -> T
> transpose i p = fromInt (toInt p + i)

\end{haskelllisting}

\begin{exercise}
Show that\ \ \code{toInt\ .\ fromInt = id}, and,
up to enharmonic equivalences,\newline \code{fromInt\ .\ toInt = id}.
\end{exercise}

\begin{exercise}
Show that\ \ \code{transpose i (transpose j p) = transpose (i+j) p}.
\end{exercise}
