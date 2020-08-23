\subsection{Conversion functions with default settings}
\seclabel{fancy-performance}

{\small
\begin{haskelllisting}

> module Haskore.Performance.Fancy where

> import qualified Haskore.Music       as Music
> import qualified Haskore.Performance as Performance
> import qualified Haskore.Performance.Context as Context
> import qualified Haskore.Performance.Player  as Player
> import qualified Haskore.Performance.Default as DefltPf

> import Haskore.Performance (eventDur, )

> -- import qualified Data.EventList.Relative.TimeBody  as TimeList
> -- import qualified Data.EventList.Relative.TimeTime  as TimeListPad
> import qualified Data.EventList.Relative.MixedTime as TimeListPad
> import qualified Data.EventList.Relative.BodyTime  as BodyTimeList

> import Control.Monad.Trans.State  (state, evalState, )
> import Control.Monad.Trans.Reader (local, )
>
> import qualified Numeric.NonNegative.Class   as NonNeg
> import qualified Numeric.NonNegative.Wrapper as NonNegW

> import Prelude hiding (map)

\end{haskelllisting}
}

\begin{figure}
{\small
\begin{haskelllisting}

> player :: (NonNeg.C time, Fractional time, Real time, Fractional dyn) =>
>    Player.T time dyn note
> player = map "Fancy"
>
> -- a PMap that makes everything into a fancyPlayer
> map ::
>    (NonNeg.C time, Fractional time, Real time, Fractional dyn) =>
>    String -> Player.T time dyn note
> map pname =
>    Performance.PlayerCons {
>       Performance.name            = pname,
>       Performance.playNote        = DefltPf.playNote,
>       Performance.interpretPhrase = fancyInterpretPhrase,
>       Performance.notatePlayer    = DefltPf.notatePlayer ()
>    }
>
> processPerformance :: (Num time) =>
>    (time ->
>       (time -> time -> time,
>        time -> Performance.Event time dyn note -> Performance.Event time dyn note,
>        time)) ->
>    (Performance.PaddedWithRests time dyn note, time) ->
>    (Performance.PaddedWithRests time dyn note, time)
> processPerformance f (pf, dur) =
>    let (fTime, fEvent, newDur) = f dur
>        procPf =
>           flip evalState 0 .
>           BodyTimeList.mapM
>              (\dt -> state $ \t -> (fTime  t dt, t+dt))
>              (\ev -> state $ \t -> (fmap (fEvent t) ev, t))
>    in  (TimeListPad.mapTimeTail procPf pf, newDur)
>
> fancyInterpretDynamic ::
>    (Fractional time, Real time, Fractional dyn) =>
>    Music.Dynamic -> Performance.Monad time dyn note -> Performance.Monad time dyn note
> fancyInterpretDynamic dyn =
>  let loud x = local (Performance.updateDynamics (fromRational x *))
>      inflate add x dur =
>         let r = fromRational x / realToFrac dur
>         in  (const id,
>              \t -> Player.changeVelocity (add (realToFrac t * r)),
>              dur)
>  in  case dyn of
>         Music.Accent x       -> Player.accent x
>         Music.Loudness x     -> loud x
>         Music.Crescendo x    -> fmap (processPerformance (inflate (+)      x))
>         Music.Diminuendo x   -> fmap (processPerformance (inflate subtract x))
> --        Music.Crescendo x    -> fmap (processPerformance (inflate x))
> --        Music.Diminuendo x   -> fmap (processPerformance (inflate (-x)))
>
> fancyInterpretTempo :: (Fractional time, Real time) =>
>    Music.Tempo -> Performance.Monad time dyn note -> Performance.Monad time dyn note
> fancyInterpretTempo tmp =
>  let stretch add x dur =
>         let x' = fromRational x
>             r = x' / dur
>             fac t dt = add 1 (r * (2*t + dt))
>         in  (\t dt -> dt * fac t dt,
>              \t (e@Performance.Event {eventDur = d}) ->
>                 e{eventDur = d * fac t d },
>              dur * add 1 x')
>  in  case tmp of
>         Music.Ritardando  x  -> fmap (processPerformance (stretch (+) x))
>         Music.Accelerando x  -> fmap (processPerformance (stretch (-) x))
> --        Music.Accelerando x  -> fmap (processPerformance (stretch (\a b -> if a>=b then a-b else 0) x))

> fancyInterpretArticulation :: (NonNeg.C time, Fractional time) =>
>    Music.Articulation -> Performance.Monad time dyn note -> Performance.Monad time dyn note
> fancyInterpretArticulation art =
>    case art of
>       Music.Staccato x -> Player.staccatoAbs x
>       Music.Legato   x -> Player.legatoAbs   x
>       Music.Slurred  x -> Player.slurredAbs  x
>       _ -> id
>         {- Remaining articulations:
>              Tenuto | Marcato | Pedal | Fermata  | FermataDown
>            | Breath | DownBow | UpBow | Harmonic | Pizzicato
>            | LeftPizz | BartokPizz | Swell | Wedge | Thumb | Stopped -}

> fancyInterpretOrnament :: (Fractional time, Real time) =>
>    Music.Ornament -> Performance.Monad time dyn note -> Performance.Monad time dyn note
> fancyInterpretOrnament _orn = id
>    {- Remaining ornamenations:
>         Trill | Mordent | InvMordent | DoubleMordent | Turn
>       | TrilledTurn | ShortTrill | Arpeggio | ArpeggioUp
>       | ArpeggioDown | Instruction String | Head NoteHead -}
>     {- Design Problem: To do these right we need to keep the KEY SIGNATURE
>        around so that we can determine, for example, what the trill note is.
>        Alternatively, provide an argument to Trill to carry this info. -}

> fancyInterpretPhrase ::
>    (NonNeg.C time, Fractional time, Real time, Fractional dyn) =>
>    Performance.PhraseFun time dyn note
> fancyInterpretPhrase pa =
>    case pa of
>       Music.Dyn dyn -> fancyInterpretDynamic dyn
>       Music.Tmp tmp -> fancyInterpretTempo tmp
>       Music.Art art -> fancyInterpretArticulation art
>       Music.Orn orn -> fancyInterpretOrnament orn

> context ::
>    (NonNeg.C time, Fractional time, Real time, Fractional dyn) =>
>    Context.T time dyn note
> context = DefltPf.context {Performance.contextPlayer = player}

\end{haskelllisting}
}
\caption{Definition of Player \function{Fancy.player}.}
\figlabel{fancy-Player}
\end{figure}


{\small
\begin{haskelllisting}

> fromMusic ::
>    (Ord note, NonNeg.C time, RealFrac time, Fractional dyn, Ord dyn) =>
>    Music.T note -> Performance.T time dyn note
> fromMusic =
>    Performance.fromMusic map context
>
> fromMusicModifyContext ::
>    (Ord note, NonNeg.C time, RealFrac time, Fractional dyn, Ord dyn) =>
>    (Context.T time dyn note -> Context.T time dyn note) ->
>    Music.T note ->
>    Performance.T time dyn note
> fromMusicModifyContext update =
>    Performance.fromMusic
>       map
>       (update context)
>
> floatFromMusic :: (Ord note) =>
>    Music.T note -> Performance.T NonNegW.Float Float note
> floatFromMusic = fromMusic
>
> paddedFromMusic  ::
>    (Ord note, NonNeg.C time, RealFrac time, Fractional dyn, Ord dyn) =>
>    Music.T note -> Performance.Padded time dyn note
> paddedFromMusic =
>    Performance.paddedFromMusic map context
>
> doublePaddedFromMusic  ::
>    (Ord note) =>
>    Music.T note -> Performance.Padded NonNegW.Double Double note
> doublePaddedFromMusic =
>    Performance.paddedFromMusic map context
>
> paddedFromMusicModifyContext ::
>    (Ord note, NonNeg.C time, RealFrac time, Fractional dyn, Ord dyn) =>
>    (Context.T time dyn note -> Context.T time dyn note) ->
>    Music.T note ->
>    Performance.T time dyn note
> paddedFromMusicModifyContext update =
>    Performance.fromMusic
>       map
>       (update context)

\end{haskelllisting}
}



% fromRhythmicMusic  :: (Ord drum, Ord instr, RealFrac time) =>
%    RhyMusic.T drum instr -> Performance.T time (RhyMusic.Note drum instr)
% fromRhythmicMusic =
%    Performance.fromMusic map context
%
% floatFromRhythmicMusic :: (Ord drum, Ord instr) =>
%    RhyMusic.T drum instr -> Performance.T Float (RhyMusic.Note drum instr)
% floatFromRhythmicMusic = fromRhythmicMusic
%
% stateFromRhythmicMusic ::
%    (Ord drum, Ord instr, Fractional time, Real time) =>
%    (RhyMusic.T drum instr) ->
%      ((Performance.T time (RhyMusic.Note drum instr), time),
%       Context.T time (RhyMusic.Note drum instr))
% stateFromRhythmicMusic m =
%    runState (Performance.monadFromMusic map m) context

% monadFromMusic ::
%    (Ord note, RealFrac time) =>
%    Music.T note -> 
%      ((Performance.T time dyn note, time),
%       Context.T time dyn note)
% monadFromMusic m =
%    runReader (Performance.monadFromMusic map m) context
