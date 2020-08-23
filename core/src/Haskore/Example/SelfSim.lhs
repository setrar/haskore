\subsection{Self-Similar (Fractal) Music.T}
\seclabel{self-similar}

\begin{haskelllisting}

> module Haskore.Example.SelfSim where
>
> import qualified Haskore.Basic.Pitch as Pitch
> import qualified Haskore.Melody as Melody
> import qualified Haskore.Music  as Music
> import           Haskore.Music.GeneralMIDI as MidiMusic
> import qualified Haskore.Interface.MIDI.Render as Render
> import qualified Sound.MIDI.File   as MidiFile

\end{haskelllisting}

An example of self-similar, or fractal, music.

\begin{haskelllisting}

> data Cluster = Cl SNote [Cluster]  -- this is called a Rose tree
> type Pat     = [SNote]
> type SNote   = [(Pitch.Absolute,Dur)]    -- i.e. a chord
>
> sim :: Pat -> [Cluster]
> sim pat = map mkCluster pat
>     where mkCluster notes = Cl notes (map (mkCluster . addmult notes) pat)
>
>
> addmult :: (Num a, Num b) => [(a, b)] -> [(a, b)] -> [(a, b)]
> addmult pds iss = zipWith addmult' pds iss
>                   where addmult' (p,d) (i,s) = (p+i,d*s)
>
> simFringe :: (Num a, Eq a) => a -> Pat -> [SNote]
> simFringe n pat = fringe n (Cl [(0,0)] (sim pat))
>
> fringe :: (Num a, Eq a) => a -> Cluster -> [SNote]
> fringe 0 (Cl n _)   = [n]
> fringe m (Cl _ cls) = concatMap (fringe (m-1)) cls
>
> -- this just converts the result to Haskore:
> simToHask :: [[(Pitch.Absolute, Music.Dur)]] -> Melody.T ()
> simToHask s = let mkNote (p,d) = Melody.note (Pitch.fromInt p) d ()
>               in line (map (chord . map mkNote) s)
>
> -- and here are some examples of it being applied:
>
> sim4 :: Int -> Melody.T ()
> sim1, sim2, sim12, sim3, sim4s :: Int -> MidiMusic.T
> t6, t7, t8, t9, t10 :: MidiFile.T
>
> sim1 n = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticBass
>            (transpose (-12)
>               (changeTempo 4 (simToHask (simFringe n pat1))))
> t6 = Render.generalMidiDeflt (sim1 4)
>
> sim2 n = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano
>            (transpose 5
>               (changeTempo 4 (simToHask (simFringe n pat2))))
> t7 = Render.generalMidiDeflt (sim2 4)
>
> sim12 n = sim1 n =:= sim2 n
> t8 = Render.generalMidiDeflt (sim12 4)
>
> sim3 n = MidiMusic.fromMelodyNullAttr MidiMusic.Vibraphone
>            (transpose 0
>               (changeTempo 4 (simToHask (simFringe n pat3))))
> t9 = Render.generalMidiDeflt (sim3 3)
>
> sim4 n  = (transpose 12
>               (changeTempo 2 (simToHask (simFringe n pat4'))))
>
> sim4s n = let s = sim4 n
>               l1 = MidiMusic.fromMelodyNullAttr MidiMusic.Flute s
>               l2 = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticBass
>                       (transpose (-36) (Music.reverse s))
>           in  l1 =:= l2
>
> ss :: MidiMusic.T
> ss     = sim4s 3
> durss :: Music.Dur
> durss  = Music.dur ss
>
> t10    = Render.generalMidiDeflt ss
>
> pat1, pat2, pat3, pat4, pat4' :: [SNote]
> pat1 = [[(0,1.0)],[(4,0.5)],[(7,1.0)],[(5,0.5)]]
> pat2 = [[(0,0.5)],[(4,1.0)],[(7,0.5)],[(5,1.0)]]
> pat3 = [[(2,0.6)],[(5,1.3)],[(0,1.0)],[(7,0.9)]]
> pat4' = [[(3,0.5)],[(4,0.25)],[(0,0.25)],[(6,1.0)]]
> pat4 = [[(3,0.5),(8,0.5),(22,0.5)],[(4,0.25),(7,0.25),(21,0.25)],
>         [(0,0.25),(5,0.25),(15,0.25)],[(6,1.0),(9,1.0),(19,1.0)]]

\end{haskelllisting}
