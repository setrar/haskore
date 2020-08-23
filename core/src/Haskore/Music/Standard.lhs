A common instance of Music.T.

> module Haskore.Music.Standard
>    (T, Note, NoteBody, Instr, Drum,
>     RhyMusic.velocity, RhyMusic.body,
>     RhyMusic.instrument, RhyMusic.pitch, RhyMusic.drum,
>     RhyMusic.noteFromStdMelodyNote,
>     fromStdMelody, fromMelodyNullAttr,
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

> import           Haskore.Basic.Duration           hiding (T)
> import           Haskore.Music           as Music hiding (T)
> import qualified Haskore.Music.Rhythmic  as RhyMusic
> import qualified Haskore.Melody          as Melody
> import qualified Haskore.Melody.Standard as StdMelody

> type Instr = String
> type Drum  = String

> type Note     = RhyMusic.Note     Drum Instr
> type NoteBody = RhyMusic.NoteBody Drum Instr
> type T        = RhyMusic.T        Drum Instr

> -- | in contrast to RhyMusic.fromStdMelody it has fixed instrument type
> fromStdMelody :: Instr -> StdMelody.T -> T
> fromStdMelody = RhyMusic.fromStdMelody

> fromMelodyNullAttr :: Instr -> Melody.T () -> T
> fromMelodyNullAttr = RhyMusic.fromMelodyNullAttr
