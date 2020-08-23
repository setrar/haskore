\subsubsection{Music}
\seclabel{music}

\begin{haskelllisting}

> module Haskore.Music where

> import qualified Haskore.Basic.Pitch    as Pitch
> import qualified Haskore.Basic.Duration as Duration

> import qualified Medium.Temporal as Temporal
> import qualified Medium.Controlled as CtrlMedium
> import qualified Medium.Controlled.List as CtrlMediumList
> import qualified Medium
> import Medium (prim, serial, parallel)

> import Haskore.General.Utility (maximum0, )
> import Data.Tuple.HT (mapPair, mapSnd, )
> import Data.Maybe.HT (toMaybe, )
> import Data.Maybe (isJust, )
> import qualified Data.List as List

\end{haskelllisting}
Melodies consist essentially of the musical atoms notes and rests.

\begin{haskelllisting}

> type Dur = Duration.T

> type Atom note = Maybe note

\end{haskelllisting}

If the atom is \code{Nothing} then it means a rest,
if it is \code{Just} it contains a note.
A note is described by its pitch and
a list of \code{NoteAttribute}s (defined later).
Both notes and rests have a duration of type \type{Dur},
which is a rational \secref{discussion:dur}.
The duration is measured in ratios of whole notes.

Notes and rests along with the duration
are put into the \type{Primitive} type.

\begin{haskelllisting}

> data Primitive note =
>          Atom Dur (Atom note) -- a note or a rest
>     deriving (Show, Eq, Ord)

\end{haskelllisting}

A primitive can not only be an atom
but also a controller as defined below.
We had to make controllers alternatives of \constructor{Atom}s
because the \type{Medium} type doesn't support them
and it would damage the beauty of \type{Medium}
if we add it at the same level as parallel and serial compositions.

\begin{haskelllisting}

> data Control =
>          Tempo      DurRatio        -- scale the tempo
>        | Transpose  Pitch.Relative  -- transposition
>        | Player     PlayerName      -- player label
>        | Phrase     PhraseAttribute -- phrase attribute
>     deriving (Show, Eq, Ord)
>
> type DurRatio   = Dur
> type PlayerName = String

> atom :: Dur -> Atom note -> T note
> atom d' = prim . Atom d'
> control :: Control -> T note -> T note
> control ctrl = CtrlMedium.control ctrl

> mkControl :: (a -> Control) -> (a -> T note -> T note)
> mkControl ctrl = control . ctrl
> changeTempo :: DurRatio -> T note -> T note
> changeTempo = mkControl Tempo
> transpose :: Pitch.Relative -> T note -> T note
> transpose = mkControl Transpose
> setPlayer :: PlayerName -> T note -> T note
> setPlayer = mkControl Player
> phrase :: PhraseAttribute -> T note -> T note
> phrase = mkControl Phrase

\end{haskelllisting}

\begin{itemize}
\item \code{changeTempo a m} scales the rate at which
\code{m} is played (i.e.\ its tempo) by a factor of \code{a}.
\item \code{transpose i m} transposes \code{m} by interval \code{i} (in semitones).
\item \code{setPlayer pname m} declares that \code{m} is to be performed by
player \code{pname}.
\item \code{phrase pa m} declares that \code{m} is to be played using
the phrase attribute (described later) \code{pa}.
(cf. \secref{discussion:phrase})
\end{itemize}

From these primitives we can build more complex musical objects.
They are captured by the \code{Music.T} datatype:
\footnote{I prefer to call these ``musical objects''
rather than ``musical values''
because the latter may be confused with musical aesthetics.}

\begin{haskelllisting}

> type T note = CtrlMediumList.T Control (Primitive note)
>
> infixr 7 +:+  {- like multiplication -}
> infixr 6 =:=  {- like addition -}
> -- make them visible for importers of Music
> (+:+), (=:=) :: T note -> T note -> T note
> (+:+) = (Medium.+:+)
> (=:=) = (Medium.=:=)

\end{haskelllisting}

\begin{itemize}
\item Musical objects can be composed sequentially
by \function{Medium.serial} or by \function{(+:+)}.
That is both \code{serial [m0, m1]} and \code{m0 +:+ m1}
denote that \code{m0} and \code{m1} are played in sequence.
(cf. \secref{discussion:media})
\item Similarly \code{Medium.parallel} and \function{(=:=)}
compose parallely.
E.g.\ both \code{parallel [m0, m1]} and \code{m0 =:= m1}
mean that \code{m0} and \code{m1} are played simultaneously.
\end{itemize}

It is convenient to represent these ideas in Haskell
as a recursive datatype rather then simple function calls
because we wish to not only construct musical objects,
but also take them apart, analyze their structure, print them in a
structure-preserving way, interpret them for performance purposes,
etc.
Nonetheless using functions that are mapped to constructors
has the advantage that song descriptions
can stay independent from a particular music data structure.

% durations and formatting of durations
\input{Haskore/Basic/Duration.lhs}

\subsubsection{Rests}
\seclabel{rests}

\begin{figure}
\begin{haskelllisting}

> rest :: Dur -> T note
> rest d' = prim (Atom d' Nothing)
>
> bnr, wnr, hnr, qnr, enr, snr, tnr, sfnr :: T note
> dwnr, dhnr, dqnr, denr, dsnr, dtnr      :: T note
> ddhnr, ddqnr, ddenr                     :: T note
>
> bnr   = rest Duration.bn     -- brevis rest
> wnr   = rest Duration.wn     -- whole note rest
> hnr   = rest Duration.hn     -- half note rest
> qnr   = rest Duration.qn     -- quarter note rest
> enr   = rest Duration.en     -- eight note rest
> snr   = rest Duration.sn     -- sixteenth note rest
> tnr   = rest Duration.tn     -- thirty-second note rest
> sfnr  = rest Duration.sfn    -- sixty-fourth note rest
>
> dwnr  = rest Duration.dwn    -- dotted whole note rest
> dhnr  = rest Duration.dhn    -- dotted half note rest
> dqnr  = rest Duration.dqn    -- dotted quarter note rest
> denr  = rest Duration.den    -- dotted eighth note rest
> dsnr  = rest Duration.dsn    -- dotted sixteenth note rest
> dtnr  = rest Duration.dtn    -- dotted thirty-second note rest
>
> ddhnr = rest Duration.ddhn  -- double-dotted half note rest
> ddqnr = rest Duration.ddqn  -- double-dotted quarter note rest
> ddenr = rest Duration.dden  -- double-dotted eighth note rest

\end{haskelllisting}
\caption{Convenient rest definitions.}
\figlabel{durations-rests}
\end{figure}

\subsubsection{Some Simple Examples}
\seclabel{basic-examples}

With this modest beginning, we can already express quite a few musical
relationships simply and effectively.

\paragraph*{Lines and Chords.}

Two common ideas in music are the construction of notes in a
horizontal fashion (a \keyword{line} or \keyword{melody}), and in a vertical
fashion (a \keyword{chord}):
\begin{haskelllisting}

> line, chord :: [T note] -> T note
> line  = serial
> chord = parallel

\end{haskelllisting}

\paragraph*{Delay and Repeat.}

Suppose now that we wish to describe a melody \code{m} accompanied by
an identical voice a perfect 5th higher.  In Haskore we simply write
``\code{m =:= transpose 7 m}''.  Similarly, a canon-like structure
involving \code{m} can be expressed as ``\code{m =:= delay d m}'',
where:
\begin{haskelllisting}

> delay :: Dur -> T note -> T note
> delay d' m = if d' == 0 then m else rest d' +:+ m

\end{haskelllisting}

Of course, Haskell's non-strict semantics also allows us to define
infinite musical objects.  For example, a musical object may be
repeated \keyword{ad nauseum} using this simple function:
\begin{haskelllisting}

> repeat :: T note -> T note
> repeat m = line (List.repeat m)

\end{haskelllisting}
Thus an infinite ostinato can be expressed in this way, and then used
in different contexts that extract only the portion that's actually
needed.

A limitted loop can be defined the same way.

\begin{haskelllisting}

> replicate :: Int -> T note -> T note
> replicate n m = line (List.replicate n m)

\end{haskelllisting}


\paragraph*{Determining Duration}

It is sometimes desirable to compute the duration in beats of a
musical object; we can do so as follows:
\begin{haskelllisting}

> dur :: T note -> Dur
> dur = Temporal.dur

> instance Temporal.C (Primitive note) where
>    dur (Atom d' _) = d'
>    none d' = Atom d' Nothing

> instance Temporal.Control Control where
>    controlDur (Tempo t) d' = d' / t
>    controlDur  _        d' = d'
>    anticontrolDur (Tempo t) d' = d' * t
>    anticontrolDur  _        d' = d'

\end{haskelllisting}

However, this measurement ignores the temporal effects
of phrases like ritardando.


\paragraph*{Super-retrograde.}

Using \code{dur} we can define a function \function{reverse}
that reverses any \code{Music.T} value
(and is thus considerably more useful than \code{retro} defined earlier).
Note the tricky treatment of parallel compositions.
Also note that this version wastes time.
It computes the duration of smaller structures
in the case of parallel compositions.
When it descends into a structure of which it has computed the duration
it computes the duration of its sub-structures again.
This can lead to a quadratic time consumption.
\begin{haskelllisting}

> reverse :: T note -> T note
> reverse = mapList
>    (,)
>    (flip const)
>    List.reverse
>    (\ms -> let durs = map dur ms
>                dmax = maximum0 durs
>            in  zipWith (delay . (dmax -)) durs ms)

\end{haskelllisting}

\paragraph*{Truncating Parallel Composition}

Note that the duration of \code{m0 =:= m1} is the maximum of the
durations of {\\code{m0} and \code{m1} (and thus if one is infinite, so
is the result).  Sometimes we would rather have the result be of
duration equal to the shorter of the two.  This is not as easy as it
sounds, since it may require interrupting the longer one in the middle
of a note (or notes).

We will define a ``truncating parallel composition'' operator \code{(/=:)},
but first we will define an auxiliary function \function{Music.take}
such that \expression{Music.take d m}
is the musical object \code{m} ``cut short'' to have at most duration \code{d}.
The name matches the one of the \module{List}
because the function is quite similar.
\begin{haskelllisting}

> take :: Dur -> T note -> T note
> take newDur m =
>    if newDur < 0
>    then error ("Music.take: newDur " ++ show newDur ++ " must be non-negative")
>    else snd (take' newDur m)

> takeLine :: Dur -> [T note] -> [T note]
> takeLine newDur = snd . takeLine' newDur

> take' :: Dur -> T note -> (Dur, T note)
> take' 0 = const (0, rest 0)
> take' newDur =
>    switchList
>       (\oldDur at -> let takenDur = min oldDur newDur
>                      in (takenDur, atom takenDur at))
>       (\ctrl -> case ctrl of
>           Tempo t -> mapPair ((/t), changeTempo t) .
>                                take' (newDur * t)
>           _       -> mapSnd  (control ctrl) .
>                                take' newDur)
>       (mapSnd line . takeLine' newDur)
>       (mapPair (maximum0,chord) . unzip . map (take' newDur))

> takeLine' :: Dur -> [T note] -> (Dur, [T note])
> takeLine' 0 _  = (0, [])
> takeLine' _ [] = (0, [])
> takeLine' newDur (m:ms) =
>    let m'  = take' newDur m
>        ms' = takeLine' (newDur - fst m') ms
>    in  (fst m' + fst ms', snd m' : snd ms')

\end{haskelllisting}
Note that \code{Music.take} is ready to handle
a \type{Music.T} object of infinite length.
The implementation of \function{takeLine'} and \function{take'} would be simpler
if one does not compute the duration of the taken part of the music in \function{take'}.
Instead one could compute the duration of the taken part where it is needed,
i.e. after \function{takeLine'} calls \function{Music.take'}.
The drawback of this simplification would be
analogously to \function{Music.reverse}:
The duration of sub-structures must be computed again and again,
which results in quadratic runtime in the worst-case.


With \code{Music.take}, the definition of \code{(/=:)} is now straightforward:
\begin{haskelllisting}

> (/=:) :: T note -> T note -> T note
> m0 /=: m1 = Haskore.Music.take (min (dur m0) (dur m1)) (m0 =:= m1)

\end{haskelllisting}
Unfortunately, whereas \code{Music.take} can handle infinite-duration music
values, \code{(/=:)} cannot.

\begin{exercise}
Define a version of \code{(/=:)} that shortens correctly when either or
both of its arguments are infinite in duration.
\end{exercise}


For completeness we want to define a function somehow dual to \function{Music.take}.
The \function{Music.drop} removes a prefix of the given duration
from the music.
Notes that begin in the removed part are lost.
This is especially important for notes which start in the removed part
and end in the remainder.
They are replaced by rests.

We would like to design \function{drop'}
such that it returns the duration of the remaining music.
This design fails for infinite music.
Thus we return the duration of the part that was dropped.
When going through a serial composition,
if we could drop less from a music item than we wanted
then the music item must have been gone completely
and must drop subsequent items.
If we dropped as much as we wanted we are ready.
If we dropped more than we wanted this indicates an error.
Remaining rests of zero duration, empty compositions and so on
may be removed by subsequent optimizations.

\begin{haskelllisting}

> drop :: Dur -> T note -> T note
> drop remDur =
>    if remDur < 0
>    then error ("Music.drop: remDur " ++ show remDur ++ " must be non-negative")
>    else snd . drop' remDur

> dropLine :: Dur -> [T note] -> [T note]
> dropLine remDur = snd . dropLine' remDur

> drop' :: Dur -> T note -> (Dur, T note)
> drop' 0 = (,) 0
> drop' remDur =
>    switchList
>       (\oldDur _ -> let newDur = min oldDur remDur
>                     in (newDur, rest (oldDur-newDur)))
>       (\ctrl -> case ctrl of
>           Tempo t -> mapPair ((/t), changeTempo t) .
>                                drop' (remDur * t)
>           _       -> mapSnd (control ctrl) .
>                                drop' remDur)
>       (mapSnd line . dropLine' remDur)
>       (mapPair (maximum0,chord) . unzip . map (drop' remDur))

> dropLine' :: Dur -> [T note] -> (Dur, [T note])
> dropLine' 0 m  = (0, m)
> dropLine' _ [] = (0, [])
> dropLine' remDur (m:ms) =
>    let (dropped, m') = drop' remDur m
>    in  case compare dropped remDur of
>          LT -> mapPair ((dropped+), id) (dropLine' (remDur - dropped) ms)
>          EQ -> (dropped, m' : ms)
>          GT -> error "dropLine': program error: dropped more than we wanted"

\end{haskelllisting}
Note that \function{mapPair} is prepared for infinite lists.

We will now define functions for filtering out notes.
This way you can e.g. extract all notes for a particular instrument.
Non-matching notes are replaced by rests.
You may want to merge them using \function{Optimization.rest}.

\begin{haskelllisting}

> filter :: (note -> Bool) -> T note -> T note
> filter p =
>    fmap (\(Atom d' mn) -> Atom d' (mn >>= \n -> toMaybe (p n) n))
> --   fmap (\(Atom d' mn) -> Atom d' (listToMaybe $ filter p $ maybeToList mn))

> partition :: (note -> Bool) -> T note -> (T note, T note)
> partition p =
>    foldList
>       (\ d' mn ->
>           mapPair
>              (atom d', atom d')
>              (if maybe False p mn
>                 then (mn, Nothing)
>                 else (Nothing, mn)))
>       (\k -> mapPair (control k, control k))
>       (mapPair (line,  line)  . unzip)
>       (mapPair (chord, chord) . unzip)

> partitionMaybe :: (noteA -> Maybe noteB) -> T noteA -> (T noteB, T noteA)
> partitionMaybe f =
>    foldList
>       (\ d' mn ->
>           mapPair
>              (atom d', atom d')
>              (let m = mn >>= f
>               in  if isJust m
>                     then (m, Nothing)
>                     else (Nothing, mn)))
>       (\k -> mapPair (control k, control k))
>       (mapPair (line,  line)  . unzip)
>       (mapPair (chord, chord) . unzip)

\end{haskelllisting}



\paragraph*{Inspecting a \type{Music.T}}

Here are some routines which specialize functions from \module{Medium}
to \module{Music}.

\begin{haskelllisting}

> applyPrimitive ::
>    (Dur -> Atom note -> b) ->
>    Primitive note -> b
> applyPrimitive fa (Atom d' at) = fa d' at

> switchBinary ::
>    (Dur -> Atom note -> b) ->
>    (Control -> T note -> b) ->
>    (T note -> T note -> b) ->
>    (T note -> T note -> b) ->
>    b -> T note -> b
> switchBinary fa fc fser fpar =
>    CtrlMedium.switchBinary (applyPrimitive fa) fser fpar fc

> switchList ::
>    (Dur -> Atom note -> b) ->
>    (Control -> T note -> b) ->
>    ([T note] -> b) ->
>    ([T note] -> b) ->
>    T note -> b
> switchList fa fc fser fpar =
>    CtrlMedium.switchList (applyPrimitive fa) fser fpar fc

> foldBin ::
>    (Dur -> Atom note -> b) ->
>    (Control -> b -> b) ->
>    (b -> b -> b) ->
>    (b -> b -> b) ->
>    b -> T note -> b
> foldBin fa fc fser fpar none' =
>    CtrlMedium.foldBin (applyPrimitive fa) fser fpar fc none'

> foldList ::
>    (Dur -> Atom note -> b) ->
>    (Control -> b -> b) ->
>    ([b] -> b) ->
>    ([b] -> b) ->
>    T note -> b
> foldList fa fc fser fpar =
>    CtrlMedium.foldList (applyPrimitive fa) fser fpar fc

> mapListFlat ::
>    (Dur -> Atom noteA -> (Dur, Atom noteB)) ->
>    (Control -> T noteA -> T noteB) ->
>    ([T noteA] -> [T noteB]) ->
>    ([T noteA] -> [T noteB]) ->
>    T noteA -> T noteB

> mapListFlat fa fc fser fpar =
>    CtrlMediumList.mapListFlat (uncurry Atom . applyPrimitive fa) fser fpar fc

> mapList ::
>    (Dur -> Atom noteA -> (Dur, Atom noteB)) ->
>    (Control -> T noteB -> T noteB) ->
>    ([T noteB] -> [T noteB]) ->
>    ([T noteB] -> [T noteB]) ->
>    T noteA -> T noteB

> mapList fa fc fser fpar =
>    CtrlMediumList.mapList (uncurry Atom . applyPrimitive fa) fser fpar fc

> -- Could be an instance of fmap if Music.T would be an algebraic type.
> mapNote :: (noteA -> noteB) -> T noteA -> T noteB
> mapNote f' = fmap (\(Atom d' at) -> Atom d' (fmap f' at))

> {-
> This is useful for duration dependend attributes,
> and duration dependend instrument sounds.
> However it seems to be more appropriate to pass the duration in seconds
> to the sound generators rather than the relative duration.
> -}
> mapDurNote :: (Dur -> noteA -> noteB) -> T noteA -> T noteB
> mapDurNote f' = fmap (\(Atom d' at) -> Atom d' (fmap (f' d') at))

\end{haskelllisting}



\input{Haskore/Composition/Trill.lhs}

\input{Haskore/Composition/Drum.lhs}  % needs \code{roll} from Trill

\subsubsection{Phrasing and Articulation}
\seclabel{phrasing}

The \code{Phrase} constructor permits
one to annotate an entire musical object with a \code{PhraseAttribute}.
This attribute datatype covers a
wide range of attributions found in common practice notation, and is
shown in \figref{attributes}.  Beware that use of them requires
the use of a player that knows how to interpret them!  Players will be
described in more detail in \secref{players}.

\begin{figure}
\begin{haskelllisting}

> data PhraseAttribute = Dyn Dynamic
>                      | Tmp Tempo
>                      | Art Articulation
>                      | Orn Ornament
>      deriving (Eq, Ord, Show)
>
> data Dynamic = Loudness Rational | Accent Rational
>              | Crescendo Rational | Diminuendo Rational
>      deriving (Eq, Ord, Show)
>
> data Tempo = Ritardando Rational | Accelerando Rational
>      deriving (Eq, Ord, Show)
>
> data Articulation = Staccato Dur | Legato Dur | Slurred Dur
>                   | Tenuto | Marcato | Pedal | Fermata | FermataDown | Breath
>                   | DownBow | UpBow | Harmonic | Pizzicato | LeftPizz
>                   | BartokPizz | Swell | Wedge | Thumb | Stopped
>      deriving (Eq, Ord, Show)
>
> data Ornament = Trill | Mordent | InvMordent | DoubleMordent
>               | Turn | TrilledTurn | ShortTrill
>               | Arpeggio | ArpeggioUp | ArpeggioDown
>               | Instruction String | Head NoteHead
>      deriving (Eq, Ord, Show)
>
> -- this is more a note attribute than a phrase attribute
> data NoteHead = DiamondHead | SquareHead | XHead | TriangleHead
>               | TremoloHead | SlashHead | ArtHarmonic | NoHead
>      deriving (Eq, Ord, Show)

\end{haskelllisting}
\caption{Note and Phrase Attributes.}
\figlabel{attributes}
\end{figure}

Again, to stay independent from the underlying data structure
we define some functions that simplify the application of several phrases.

\begin{haskelllisting}

> dynamic :: Dynamic -> T note -> T note
> dynamic = phrase . Dyn

> tempo :: Tempo -> T note -> T note
> tempo = phrase . Tmp

> articulation :: Articulation -> T note -> T note
> articulation = phrase . Art

> ornament :: Ornament -> T note -> T note
> ornament = phrase . Orn


> accent, crescendo, diminuendo, loudness1,
>    ritardando, accelerando ::
>       Rational -> T note -> T note

> accent     = dynamic . Accent
> crescendo  = dynamic . Crescendo
> diminuendo = dynamic . Diminuendo
> loudness1  = dynamic . Loudness

> ritardando  = tempo . Ritardando
> accelerando = tempo . Accelerando

> staccato, legato :: Dur -> T note -> T note
>
> staccato = articulation . Staccato
> legato   = articulation . Legato

\end{haskelllisting}


Note that some of the attributes are parameterized with a numeric value.
This is used by a player to control the degree to which
an articulation is to be applied.
For example the articulations \constructor{Staccato}, \constructor{Legato},
\constructor{Slurred} describe the overlapping between notes.
We would expect \code{Legato 1.2}
to create more of a legato feel than \code{Legato 1.1},
and \code{Staccato 2} to be stronger than \code{Staccato 1}.

The following constants represent default values for some of the
parameterized attributes:
\begin{haskelllisting}

> defltLegato, defltStaccato,
>   defltAccent, bigAccent :: T note -> T note
>
> defltLegato    = legato   Duration.sn
> defltStaccato  = staccato Duration.sn
> defltAccent    = accent 1.2
> bigAccent      = accent 1.5

\end{haskelllisting}

To understand exactly how a player interprets an attribute requires
knowing how players are defined.  Haskore defines only a few simple
players, so in fact many of the attributes in \figref{attributes}
are to allow the user to give appropriate interpretations of them by
her particular player.  But before looking at the structure of players
we will need to look at the notion of a \keyword{performance} (these two
ideas are tightly linked, which is why the \code{Player} and \code{Performance}
modules are mutually recursive).


\begin{exercise}
Find a simple piece of music written by your favorite composer, and
transcribe it into Haskore.  In doing so, look for repeating patterns,
transposed phrases, etc. and reflect this in your code, thus revealing
deeper structural aspects of the music than that found in common
practice notation.
\end{exercise}

\secref{chick} shows the first 28 bars of Chick Corea's
``Children's Song No.~6'' encoded in Haskore.
