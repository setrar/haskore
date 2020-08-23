
\subsection{Optimization}
\seclabel{optimization}

This module provides functions that simplify the structure
of a \code{Music.T} according to the rules proven in
\secref{equivalence}

\begin{haskelllisting}

> module Haskore.Process.Optimization where

> import qualified Medium.Controlled.List as CtrlMediumList
> import qualified Medium.Controlled      as CtrlMedium
> import qualified Haskore.Music as Music
> import Medium.Controlled.List (serial, parallel, )
> import Data.List.HT (partitionMaybe, )
> import Data.Maybe.HT (toMaybe, )
> import Data.Maybe (catMaybes, fromMaybe, )

\end{haskelllisting}

\code{Music.T} objects that come out of \code{ReadMidi.toMusic}
almost always contain redundancies,
like rests of zero duration and redundant instrument specifications.
The function \function{Optimization.all}
reduces the redundancy to make a \type{Music.T} file
less cluttered and more efficient to use.
\begin{haskelllisting}

> all, rest, composition, duration, tempo, transpose, volume ::
>    Music.T note -> Music.T note
> all = tempo . transpose . volume . singleton . composition . rest

\end{haskelllisting}

Remove rests of zero duration.
\begin{haskelllisting}

> rest = Music.mapList
>    (,)
>    (flip const)
>    (filter (not . isZeroRest))
>    (filter (not . isZeroRest))

> isZeroRest :: Music.T note -> Bool
> isZeroRest =
>    Music.switchList
>       (\d at -> d==0 && maybe True (const False) at)
>       (const (const False))
>       (const False)
>       (const False)

\end{haskelllisting}

Remove empty parallel and serial compositions
and controllers of empty music.
\begin{haskelllisting}

> composition = fromMaybe (Music.rest 0) . Music.foldList
>    (\d -> Just . Music.atom d)
>    (fmap . Music.control)
>    ((\ms -> toMaybe (not (null ms)) (serial   ms)) . catMaybes)
>    ((\ms -> toMaybe (not (null ms)) (parallel ms)) . catMaybes)

\end{haskelllisting}

Remove any atom of zero duration.
This is not really an optimization but a hack to get rid
of MIDI NoteOn and NoteOff events at the same time point.
\begin{haskelllisting}

> duration = fromMaybe (Music.rest 0) . Music.foldList
>    (\d -> toMaybe (d /= 0) . Music.atom d)
>    (fmap . Music.control)
>    (Just . serial   . catMaybes)
>    (Just . parallel . catMaybes)

\end{haskelllisting}

The control structures for tempo, transposition and change of instruments
can be handled very similar using the following routines.
The function \function{mergeControl'} checks
if nested controllers are of the same kind.
If they are then they are merged into one.
The function would be much simpler
if it would be implemented for specific constructors,
but we want to stay independent from the particular data structure,
which is already quite complex.
\begin{haskelllisting}

> mergeControl' ::
>       (Music.Control -> Maybe a)
>    -> (a -> Music.T note -> Music.T note)
>    -> (a -> a -> a)
>    -> Music.T note
>    -> Music.T note
> mergeControl' extract control merge =
>    let fcSub c m = fmap (flip (,) m) (extract c)
>        fc' c0 m0 x0 =
>                 maybe (Music.control c0 m0)
>                       (\(x1,m1) -> control (merge x0 x1) m1)
>                       (Music.switchList (const (const Nothing))
>                           fcSub (const Nothing) (const Nothing) m0)
>        fc c m = maybe (Music.control c m)
>                       (fc' c m)
>                       (extract c)
>    in  Music.foldList
>           Music.atom fc Music.line Music.chord

\end{haskelllisting}

The following function collects neighboured controllers into groups,
extracts controllers of a specific type
and prepends a controller to the list of neighboured controllers,
which has the total effect of the extracted controllers.
This change of ordering is always possible
because in the current set of controllers
two neighboured controllers of different type commutes.
E.g. it is
\code{transpose n . changeTempo r == changeTempo r . transpose n}
and thus the following simplification
\code{transpose 1 . changeTempo 2 . transpose 3 == transpose 4 . changeTempo 2}
is possible.

\begin{haskelllisting}

> mergeControl, mergeControlCompact ::
>       (Music.Control -> Maybe a)
>    -> (a -> Music.T note -> Music.T note)
>    -> (a -> a -> a)
>    -> Music.T note
>    -> Music.T note
> mergeControlCompact extract control merge =
>    let collectControl =
>           Music.switchList
>              (\d n -> ([], Music.atom d n))
>              (\c m -> let cm = collectControl m
>                       in  (c : fst cm, snd cm))
>              ((,) [] . Music.line  . map recourse)
>              ((,) [] . Music.chord . map recourse)
>        recourse m =
>           let cm = collectControl m
>               (xs, cs') = partitionMaybe extract (fst cm)
>               x  = foldl1 merge xs
>               collectedCtrl = if null xs then id else control x
>           in  collectedCtrl (foldr id (snd cm) (map Music.control cs'))
>    in  recourse

> -- more intuitive implementation
> mergeControl extract control merge =
> --   flattenControllers .
> --   CtrlMediumList.mapControl
>    CtrlMedium.foldList
>       CtrlMediumList.prim
>       CtrlMediumList.serial
>       CtrlMediumList.parallel
>       (\cs cm ->
>           let (xs, cs') = partitionMaybe extract cs
>               collectedCtrl =
>                  if null xs then id else control (foldl1 merge xs)
>           in  collectedCtrl (foldr id cm (map Music.control cs'))) .
>    cumulateControllers

> cumulateControllers ::
>       CtrlMediumList.T control a
>    -> CtrlMediumList.T [control] a
> cumulateControllers =
>    CtrlMedium.foldList
>       CtrlMediumList.prim
>       CtrlMediumList.serial
>       CtrlMediumList.parallel
>       (\c m ->
>          let cm = CtrlMedium.control [c] m
>          in  CtrlMedium.switchList
>                 (const cm)
>                 (const cm)
>                 (const cm)
>                 (\cs m' -> CtrlMedium.control (c:cs) m')
>                 m)

> flattenControllers ::
>       CtrlMediumList.T [control] a
>    -> CtrlMediumList.T control a
> flattenControllers =
>    CtrlMedium.foldList
>       CtrlMediumList.prim
>       CtrlMediumList.serial
>       CtrlMediumList.parallel
>       (flip (foldr id) . map CtrlMedium.control)

\end{haskelllisting}

The function \function{removeNeutral} removes controllers
that have no effect.
\begin{haskelllisting}

> removeNeutral :: (Music.Control -> Bool) -> Music.T note -> Music.T note
> removeNeutral isNeutral =
>    let fc c m = if isNeutral c
>                 then m
>                 else Music.control c m
>    in  Music.foldList Music.atom fc Music.line Music.chord

\end{haskelllisting}


Remove redundant \code{Tempo}s.
\begin{haskelllisting}

> tempo =
>    let maybeTempo (Music.Tempo t) = Just t
>        maybeTempo _               = Nothing
>    in  removeNeutral (== Music.Tempo 1) .
>           mergeControl maybeTempo Music.changeTempo (*)

\end{haskelllisting}

Remove redundant \code{Transpose}s.
\begin{haskelllisting}

> transpose =
>    let maybeTranspose (Music.Transpose t) = Just t
>        maybeTranspose _                   = Nothing
>    in  removeNeutral (== Music.Transpose 0) .
>           mergeControl maybeTranspose Music.transpose (+)

\end{haskelllisting}

Change repeated Volume Note Attributes to Phrase Attributes.
\begin{haskelllisting}

> volume =
>    let maybeLoudness (Music.Phrase (Music.Dyn (Music.Loudness t))) = Just t
>        maybeLoudness _ = Nothing
>    in  removeNeutral (== Music.Phrase (Music.Dyn (Music.Loudness 1))) .
>           mergeControl maybeLoudness Music.loudness1 (*)

\end{haskelllisting}

Eliminate \code{Serial} and \code{Parallel} composition
if they contain only one member.
This can be done very general for \type{CtrlMedium.T}.
We have also a version which works on \type{Music.T}.
Since the medium data type supports controllers
there is no longer a real difference between these two functions.
\begin{haskelllisting}

> singletonMedium ::
>    CtrlMediumList.T control a -> CtrlMediumList.T control a
> singletonMedium =
>    CtrlMedium.foldList CtrlMediumList.prim
>       (\ms -> case ms of {[x] -> x; _ -> serial   ms})
>       (\ms -> case ms of {[x] -> x; _ -> parallel ms})
>       (CtrlMedium.control)

> singleton :: Music.T note -> Music.T note
> singleton =
>    Music.foldList Music.atom Music.control
>       (\ms -> case ms of {[x] -> x; _ -> Music.line  ms})
>       (\ms -> case ms of {[x] -> x; _ -> Music.chord ms})

\end{haskelllisting}
