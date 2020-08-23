\subsection{Children's Song No. 6}
\seclabel{chick}

This is a partial encoding of Chick Corea's ``Children's Song No. 6''.

\begin{haskelllisting}

> module Haskore.Example.ChildSong6 where

> import           Haskore.Melody.Standard   as Melody
> import           Haskore.Music.GeneralMIDI as MidiMusic
> import qualified Haskore.Music             as Music

\end{haskelllisting}

note updaters for mappings

\begin{haskelllisting}

> fd :: t -> (t -> NoteAttributes -> m) -> m
> fd dur n = n dur v
>
> vel :: (NoteAttributes -> m) -> m
> vel  n   = n   v
>
> v :: NoteAttributes
> v        = Melody.na
>
> lmap :: (a -> Melody.T) -> [a] -> Melody.T
> lmap func l = line (map func l)
>
>
> bassLine, mainVoice :: Melody.T
> song :: MidiMusic.T

\end{haskelllisting}

Baseline:

\begin{haskelllisting}

> b1, b2, b3 :: Melody.T
> b1 = lmap (fd dqn) [b  3, fs 4, g  4, fs 4]
> b2 = lmap (fd dqn) [b  3, es 4, fs 4, es 4]
> b3 = lmap (fd dqn) [as 3, fs 4, g  4, fs 4]
>
> bassLine =
>    Music.loudness1 (10/13)
>       (line [Music.replicate 3 b1, Music.replicate 2 b2,
>              Music.replicate 4 b3, Music.replicate 5 b1])

\end{haskelllisting}

Main Voice:

\begin{haskelllisting}

> v1, v1a, v1b :: Melody.T
> v1  = v1a +:+ v1b
> v1a = lmap (fd en) [a 5, e 5, d 5, fs 5, cs 5, b 4, e 5, b 4]
> v1b = lmap vel     [cs 5 tn, d 5 (qn-tn), cs 5 en, b 4 en]
>
> v2, v2a, v2b, v2c, v2d, v2e, v2f :: Melody.T
> v2  = line [v2a, v2b, v2c, v2d, v2e, v2f]
> v2a = lmap vel [cs 5 (dhn+dhn), d 5 dhn,
>                 f 5 hn, gs 5 qn, fs 5 (hn+en), g 5 en]
> v2b = lmap (fd en) [fs 5, e 5, cs 5, as 4] +:+ a 4 dqn v +:+
>       lmap (fd en) [as 4, cs 5, fs 5, e 5, fs 5, g 5, as 5]
> v2c = lmap vel [cs 6 (hn+en), d 6 en, cs 6 en, e 5 en] +:+ enr +:+
>       lmap vel [as 5 en, a 5 en, g 5 en, d 5 qn, c 5 en, cs 5 en]
> v2d = lmap (fd en) [fs 5, cs 5, e 5, cs 5, a 4, as 4, d 5, e 5, fs 5] +:+
>       lmap vel [fs 5 tn, e 5 (qn-tn), d 5 en, e 5 tn, d 5 (qn-tn),
>                 cs 5 en, d 5 tn, cs 5 (qn-tn), b 4 (en+hn)]
> v2e = lmap vel [cs 5 en, b 4 en, fs 5 en, a 5 en, b 5 (hn+qn), a 5 en,
>                 fs 5 en, e 5 qn, d 5 en, fs 5 en, e 5 hn, d 5 hn, fs 5 qn]
> v2f = changeTempo (3/2) (lmap vel [cs 5 en, d 5 en, cs 5 en]) +:+ b 4 (3*dhn+hn) v
>
> mainVoice = Music.replicate 3 v1 +:+ v2

\end{haskelllisting}

Putting it all together:

\begin{haskelllisting}

> song = MidiMusic.fromStdMelody MidiMusic.AcousticGrandPiano
>           (transpose (-48) (changeTempo 3
>              (bassLine =:= mainVoice)))

\end{haskelllisting}
