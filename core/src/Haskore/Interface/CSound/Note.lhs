\subsubsection{The Score File}
\seclabel{score-file}

\begin{haskelllisting}

> module Haskore.Interface.CSound.Note where
>
> import qualified Haskore.Basic.Pitch           as Pitch
> import qualified Haskore.Music.Rhythmic        as RhyMusic

> import qualified Haskore.Interface.CSound.InstrumentMap as InstrMap

> import Haskore.Interface.CSound (Instrument, Velocity, PField)

\end{haskelllisting}


\begin{haskelllisting}

> data T =
>    Cons {
>      parameters :: [PField],
>      velocity   :: Velocity,
>      instrument :: Instrument,
>      pitch      :: Maybe Pitch.Absolute
>    }

> fromRhyNote :: RealFrac dyn =>
>    InstrMap.ToSound drum ->
>    InstrMap.ToSound instr ->
>       dyn -> Pitch.Relative -> RhyMusic.Note drum instr -> T
> fromRhyNote dMap iMap dyn trans (RhyMusic.Note vel body) =
>    let velCS = velocityFromStd dyn vel
>    in  case body of
>           RhyMusic.Tone instr p ->
>              uncurry (flip Cons velCS) (iMap instr)
>                      (Just (pitchFromStd trans p))
>           RhyMusic.Drum drum ->
>              uncurry (flip Cons velCS) (dMap drum) Nothing

> velocityFromStd :: RealFrac dyn =>
>    dyn -> Rational -> Velocity
> velocityFromStd dyn vel =
>    velocityToDb (fromRational (toRational dyn * vel))
> --   velocityToDb (realToFrac dyn * vel)

> pitchFromStd :: Pitch.Relative -> Pitch.T -> Pitch.Absolute
> pitchFromStd trans p =
>    let csoundP = Pitch.toInt p + zeroKey + trans
>    in  if csoundP<0
>        then error ("CSound.Note: pitch " ++ show csoundP ++
>                    " must not be negative")
>        else csoundP

\end{haskelllisting}


\begin{haskelllisting}

> velocityToDb :: Float -> Float
> velocityToDb = (50*)
>
> -- still unused, but it should be implemented this way
> amplitudeToDb :: Float -> Float
> amplitudeToDb v = 20 * logBase 10 v

> {- Offset to map from Haskore's pitch 0
>    to the corresponding pitch of CSound -}
> zeroKey :: Int
> zeroKey = 84

\end{haskelllisting}
