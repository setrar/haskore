The first phrase of the flute part of "Stars and Stripes Forever."

\begin{haskelllisting}

> module Haskore.Example.Ssf where
> import Haskore.Composition.Trill as Trill
> import Haskore.Melody            as Melody
> import Haskore.Music.GeneralMIDI as MidiMusic
>
> shortLegato :: Melody.T () -> Melody.T ()
> shortLegato = legato (sn/10)
>
> m1, m2, m3, m4 :: [Melody.T ()]
> m1 = [         trillN 2 5 (bf 2 en ()),
>       defltStaccato (line [ef 3 en (),
>                            ef 2 en (),
>                            ef 3 en ()])]
>
> m2 = [shortLegato   (line [bf 2 sn (),
>                            c  3 sn (),
>                            bf 2 sn (),
>                            g  2 sn ()]),
>       defltStaccato (line [ef 2 en (),
>                            bf 1 en ()])]
>
> m3 = [shortLegato   (line [ef 2 sn (),
>                            f  2 sn (),
>                            g  2 sn (),
>                            af 2 sn ()]),
>       defltStaccato (line [bf 2 en (),
>                            ef 3 en ()])]
>
> m4 = [         trill 2 tn (bf 2 qn ()),
>                            bf 2 sn (),
>                            denr]
>
> melody :: Melody.T ()
> melody = line (m1 ++ m2 ++ m3 ++ m4)
> song :: MidiMusic.T
> song = MidiMusic.fromMelodyNullAttr MidiMusic.Flute (changeTempo 2 melody)
