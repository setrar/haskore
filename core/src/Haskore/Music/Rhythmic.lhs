A common instance of Music.T.
It represents rhythmic music, that is melodies plus drums.
The types for melody instruments and drums can be chosen freely.
They may be plain strings, enumerations or parametrized instrument descriptions.

\begin{haskelllisting}

> module Haskore.Music.Rhythmic
>    (T, Note(..), NoteBody(..),
>     maybeInstrument,
>     noteFromAttrs, noteFromStdMelodyNote, noteFromMelodyNote,
>     fromStdMelody, fromMelodyNullAttr, fromMelody,
>
>     bn, wn, hn, qn, en, sn, tn, sfn,
>     dwn, dhn, dqn, den, dsn, dtn,
>     ddhn, ddqn, dden,
>     bnr, wnr, hnr, qnr, enr, snr, tnr, sfnr,
>     dwnr, dhnr, dqnr, denr, dsnr, dtnr,
>     ddhnr, ddqnr, ddenr,
>     line, chord, changeTempo, transpose, phrase,
>     (Music.+:+), (Music.=:=), Dur,
>
>     PhraseAttribute(..), Dynamic(..),
>     Tempo(..), Articulation(..), Ornament(..), NoteHead(..),
>     accent, crescendo, diminuendo, loudness1,
>     ritardando, accelerando, staccato, legato,
>     defltLegato, defltStaccato,
>     defltAccent, bigAccent) where

> import qualified Haskore.Basic.Pitch     as Pitch
> import           Haskore.Basic.Duration hiding (T)
> import           Haskore.Music          hiding (T, partitionMaybe)
> import qualified Haskore.Music           as Music
> import qualified Haskore.Melody          as Melody
> import qualified Haskore.Melody.Standard as StdMelody

> import qualified Data.Accessor.Basic as Accessor

> import qualified Data.Record.HT as Record
> import Data.Ord.HT (comparing, )

> data Note drum instr =
>      Note {velocity   :: Rational,
>            body       :: NoteBody drum instr}
>     deriving (Show, Eq)

\end{haskelllisting}

A note of a rhythmic music can be
either a tone of a melody instrument or a drum.
Every effect, which has no pitch, is considered as a drum.
Naturally \code{Tone}s are affected by transposition
whereas \code{Drum}s are not.

\begin{haskelllisting}

> data NoteBody drum instr =
>      Tone {instrument :: instr,
>            pitch      :: Pitch.T}
>    | Drum {drum       :: drum}
>     deriving (Show, Eq, Ord)

> -- this order is just for the old test cases which rely on it
> instance (Ord instr, Ord drum) => Ord (Note drum instr) where
>    compare =
>       Record.compare
>          [comparing body,
>           comparing velocity]

> type T drum instr = Music.T (Note drum instr)

> maybeInstrument :: NoteBody drum instr -> Maybe instr
> maybeInstrument (Tone instr _) = Just instr
> maybeInstrument (Drum _)       = Nothing

\end{haskelllisting}

A rhythmic music can be created by assigning an instrument to a melody.
The function \function{fromStdMelody} does this while preserving common note attributes,
and the function \function{fromMelodyNullAttr}
ignores the note attributes.
This is useful in case no additional attributes are needed.
In this case the \type{attr} type variable can be the null type \type{()}.

\begin{haskelllisting}

> noteFromAttrs :: StdMelody.NoteAttributes ->
>    NoteBody drum instr -> Note drum instr
> noteFromAttrs nas =
>    Note (Accessor.get StdMelody.velocity1 nas)

> noteFromStdMelodyNote :: instr -> StdMelody.Note -> Note drum instr
> noteFromStdMelodyNote instr (Melody.Note nas p) =
>    noteFromAttrs nas (Tone instr p)

> noteFromMelodyNote ::
>    (attr -> (Rational,instr)) ->
>       Melody.Note attr -> Note drum instr
> noteFromMelodyNote attrToInstr (Melody.Note x p) =
>    let (vel,instr) = attrToInstr x
>    in  Note vel (Tone instr p)

> fromStdMelody :: instr -> StdMelody.T -> T drum instr
> fromStdMelody instr = Music.mapNote (noteFromStdMelodyNote instr)

> -- | ignores the note attributes
> fromMelodyNullAttr :: instr -> Melody.T () -> T drum instr
> fromMelodyNullAttr instr =
>    fromStdMelody instr . StdMelody.fromMelodyNullAttr
> --   fromMelody (const (1,instr))

> fromMelody ::
>    (attr -> (Rational,instr)) -> Melody.T attr -> T drum instr
> fromMelody attrToInstr =
>    Music.mapNote (noteFromMelodyNote attrToInstr)

\end{haskelllisting}
