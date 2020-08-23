\subsection{Haskore in Action}
\seclabel{examples}

\begin{haskelllisting}

> module Haskore.Example.Miscellaneous where
>
> import           Haskore.Composition.Trill as Trill
> import           Haskore.Composition.Drum  as Drum
>
> import qualified Haskore.Music           as Music
> import           Haskore.Music (rest, delay, (/=:))
> import           Haskore.Music.GeneralMIDI as MidiMusic
> import qualified Haskore.Music.Rhythmic  as RhyMusic
> import qualified Haskore.Melody          as Melody
> import           Haskore.Melody.Standard as StdMelody
> import qualified Haskore.Performance.Context as Context

> import qualified Haskore.Interface.MIDI.InstrumentMap as InstrMap
> import qualified Haskore.Interface.MIDI.Write        as WriteMidi
> import qualified Haskore.Interface.MIDI.Read         as ReadMidi
> import qualified Haskore.Interface.MIDI.Render       as Render

> import qualified Sound.MIDI.File.Save    as SaveMidi
> import qualified Sound.MIDI.File.Load    as LoadMidi
> import qualified Sound.MIDI.File         as MidiFile
> import qualified Sound.MIDI.General      as GeneralMidi

> import qualified Haskore.Example.SelfSim        as SelfSim
> import qualified Haskore.Example.ChildSong6     as ChildSong6
> import qualified Haskore.Example.Ssf            as Ssf

> import           Haskore.Basic.Duration ((%+))
> import qualified Numeric.NonNegative.Wrapper as NonNeg

> import Data.Tuple.HT (fst3, snd3, thd3, )


> t0, t1, t2, t3, t4, t5,
>  t10s, t12, t12a, t13, t13a, t13b, t13c, t13d, t13e,
>  t14, t14b, t14c, t14d, cs6, ssf0 :: MidiFile.T

> piano, vibes, flute :: GeneralMidi.Instrument
> piano = GeneralMidi.AcousticGrandPiano
> vibes = GeneralMidi.Vibraphone
> flute = GeneralMidi.Flute

\end{haskelllisting}

Simple examples of Haskore in action.  Note that this module also
imports modules ChildSong6, SelfSim, and Ssf.

\vspace{2ex}
\hrule{\hfill}

From the tutorial, try things such as pr12, cMajArp, cMajChd, etc. and
try applying inversions, retrogrades, etc. on the same examples.  Also
try \code{ChildSong.song}.  For example:

\begin{haskelllisting}

> t0 = Render.generalMidiDeflt ChildSong6.song

\end{haskelllisting}

\hrule{\hfill}

C Major scale for use in examples below:

\begin{haskelllisting}

> cms', cms :: Melody.T ()
> cms' = line (map (\n -> n en ())
>           [c 0, d 0, e 0, f 0, g 0, a 0, b 0, c 1])
> cms = changeTempo 2 cms'

> drumScale :: MidiMusic.T
> drumScale =
>    line (map (\n -> Drum.toMusicDefaultAttr (toEnum (n+13)) sn)
>              [0,2,4,5,7,9,11,12])

\end{haskelllisting}

Test of various articulations and dynamics:

\begin{haskelllisting}

> t1 = Render.generalMidi
>        (staccato (sn/10) drumScale +:+
>                          drumScale +:+
>         legato   (sn/10) drumScale    )
>
> temp, mu2 :: MidiMusic.T
> temp = MidiMusic.fromMelodyNullAttr piano (crescendo 4.0 (c 0 en ()))
>
> mu2 = MidiMusic.fromMelodyNullAttr vibes
>        (diminuendo 0.75 cms +:+
>         crescendo 0.75 (loudness1 0.25 cms))
> t2 = Render.generalMidiDeflt mu2
>
> t3 = Render.generalMidiDeflt (MidiMusic.fromMelodyNullAttr flute
>        (accelerando 0.3 cms +:+
>         ritardando  0.6 cms    ))

\end{haskelllisting}

\hrule{\hfill}

A function to recursively apply transformations
\code{f'} (to elements in a sequence) and
\code{g'} (to accumulated phrases):

\begin{haskelllisting}

> rep :: (Music.T note -> Music.T note)
>     -> (Music.T note -> Music.T note)
>     -> Int -> Music.T note -> Music.T note
> rep _  _  0 _ = rest 0
> rep f' g' n m = m =:= g' (rep f' g' (n-1) (f' m))

\end{haskelllisting}

An example using "rep" three times, recursively, to create a "cascade"
of sounds.

\begin{haskelllisting}

> run, cascade, cascades :: Melody.T ()
> run       = rep (transpose 5) (delay tn) 8 (c 0 tn ())
> cascade   = rep (transpose 4) (delay en) 8 run
> cascades  = rep  id           (delay sn) 2 cascade
>
> t4' :: Melody.T () -> MidiFile.T
> t4' x     = Render.generalMidiDeflt (MidiMusic.fromMelodyNullAttr piano x)
> t4        = Render.generalMidiDeflt (MidiMusic.fromMelodyNullAttr piano
>               (cascades +:+ Music.reverse cascades))

\end{haskelllisting}

What happens if we simply reverse the \code{f} and \code{g} arguments?

\begin{haskelllisting}

> run', cascade', cascades' :: Melody.T ()
> run'      = rep (delay tn) (transpose 5) 4 (c 0 tn ())
> cascade'  = rep (delay en) (transpose 4) 6 run'
> cascades' = rep (delay sn)  id           2 cascade'
> t5        = Render.generalMidiDeflt (MidiMusic.fromMelodyNullAttr piano cascades')

\end{haskelllisting}

\hrule{\hfill}

Example from the SelfSim module.

\begin{haskelllisting}

> t10s   = Render.generalMidiDeflt (rep (delay SelfSim.durss) (transpose 4) 2 SelfSim.ss)

\end{haskelllisting}

\hrule{\hfill}

Example from the ChildSong6 module.

\begin{haskelllisting}

> cs6 = Render.generalMidiDeflt ChildSong6.song

\end{haskelllisting}

\hrule{\hfill}

Example from the Ssf (Stars and Stripes Forever) module.

\begin{haskelllisting}

> ssf0 = Render.generalMidiDeflt Ssf.song

\end{haskelllisting}

\hrule{\hfill}

Midi percussion test.  Plays all "notes" in a range.  (Requires adding
an instrument for percussion to the \code{InstrMap}.)

\begin{haskelllisting}

> drums :: GeneralMidi.Drum -> GeneralMidi.Drum -> MidiMusic.T
> drums dr0 dr1 =
>    line (map (\drm -> Drum.toMusicDefaultAttr drm sn) [dr0..dr1])
>
> t11 :: GeneralMidi.Drum -> GeneralMidi.Drum -> MidiFile.T
> t11 dr0 dr1 = Render.generalMidiDeflt (drums dr0 dr1)

\end{haskelllisting}

\hrule{\hfill}

Test of \function{Music.take} and shorten.

\begin{haskelllisting}

> t12 = Render.generalMidiDeflt (Music.take 4 ChildSong6.song)
> t12a =
>    Render.generalMidiDeflt
>       (MidiMusic.fromMelodyNullAttr piano cms /=: ChildSong6.song)

\end{haskelllisting}

\hrule{\hfill}

Tests of the trill functions.

\begin{haskelllisting}

> t13note :: MidiMusic.T
> t13note = MidiMusic.fromMelodyNullAttr piano (c 1 qn ())
> t13 =  Render.generalMidiDeflt (trill   1 sn t13note)
> t13a = Render.generalMidiDeflt (trill'  2 dqn t13note)
> t13b = Render.generalMidiDeflt (trillN  1 5 t13note)
> t13c = Render.generalMidiDeflt (trillN' 3 7 t13note)
> t13d = Render.generalMidiDeflt (roll tn t13note)
> t13e = Render.generalMidiDeflt (changeTempo (2/3) (transpose 2 (trillN' 2 7 t13note)))

\end{haskelllisting}

\hrule{\hfill}

Tests of drum.

\begin{haskelllisting}

> t14 = Render.generalMidiDeflt (Drum.toMusicDefaultAttr AcousticSnare qn)

\end{haskelllisting}

A "funk groove"

\begin{haskelllisting}

> t14b = let p1 = Drum.toMusicDefaultAttr LowTom        qn
>            p2 = Drum.toMusicDefaultAttr AcousticSnare en
>        in Render.generalMidiDeflt (changeTempo 3 (Music.replicate 4
>                  (line [p1, qnr, p2,  qnr, p2,
>                         p1, p1,  qnr, p2,  enr]
>                   =:= roll en (Drum.toMusicDefaultAttr ClosedHiHat 2))))

\end{haskelllisting}

A "jazz groove"

\begin{haskelllisting}

> t14c = let p1 = Drum.toMusicDefaultAttr CrashCymbal2  qn
>            p2 = Drum.toMusicDefaultAttr AcousticSnare en
>            p3 = Drum.toMusicDefaultAttr LowTom        qn
>        in Render.generalMidiDeflt (changeTempo 3 (Music.replicate 8
>                  ((p1 +:+ changeTempo (3%+2) (p2 +:+ enr +:+ p2))
>                   =:= (p3 +:+ qnr)) ))

> t14d = let p1 = Drum.toMusicDefaultAttr LowTom        en
>            p2 = Drum.toMusicDefaultAttr AcousticSnare hn
>        in Render.generalMidiDeflt(line [roll tn p1,
>                          p1,
>                          p1,
>                          rest en,
>                          roll tn p1,
>                          p1,
>                          p1,
>                          rest qn,
>                          roll tn p2,
>                          p1,
>                          p1]  )

\end{haskelllisting}

\hrule{\hfill}

\paragraph{Tests of the MIDI interface.}

\code{MidiMusic.T} into a MIDI file.

\begin{haskelllisting}

> tab :: MidiMusic.T -> IO ()
> tab m = SaveMidi.toFile "test.mid" (Render.generalMidiDeflt m)

\end{haskelllisting}

\code{MidiMusic.T} to a MidiFile datatype and back to Music.

\begin{haskelllisting}

> type StdContext =
>    Context.T NonNeg.Float Float (RhyMusic.Note MidiMusic.Drum MidiMusic.Instr)
> -- type StdContext = Pf.Context NonNeg.Float Float MidiMusic.Note -- rejected by Hugs

> type MidiArrange =
>    (InstrMap.ChannelTable MidiMusic.Instr, StdContext, MidiMusic.T)

> tad :: MidiMusic.T -> MidiArrange
> tad = ReadMidi.toGMMusic . Render.generalMidiDeflt

\end{haskelllisting}

A MIDI file to a MidiFile datatype and back to a MIDI file.

\begin{haskelllisting}

> tcb, tc, tcd, tcdab :: FilePath -> IO ()
> tcb file = LoadMidi.fromFile file >>= SaveMidi.toFile "test.mid"

\end{haskelllisting}

MIDI file to MidiFile datatype.

\begin{haskelllisting}

> tc file = LoadMidi.fromFile file >>= print

\end{haskelllisting}

MIDI file to \code{MidiMusic.T}, a \code{InstrMap}, and a \code{Context}.

\begin{haskelllisting}

> tcd file = do
>              x <- fmap ReadMidi.toGMMusic
>                        (LoadMidi.fromFile file)
>              print $ fst3 (x::MidiArrange)
>              print $ snd3 x
>              print $ thd3 x

\end{haskelllisting}

A MIDI file to \code{MidiMusic.T} and back to a MIDI file.

\begin{haskelllisting}

> tcdab file =
>    LoadMidi.fromFile file >>=
>       (SaveMidi.toFile "test.mid" . WriteMidi.fromGMMusic .
>          (id::MidiArrange -> MidiArrange) . ReadMidi.toGMMusic)

\end{haskelllisting}
