
\begin{haskelllisting}

> module Haskore.Melody.Standard
>           (Note, T, NoteAttributes, fromMelodyNullAttr,
>            na, velocity1, vibrato, tremolo,
>            cf,c,cs,df,d,ds,ef,e,es,ff,f,fs,gf,g,gs,af,a,as,bf,b,bs) where

> import Haskore.Melody
>           (cf,c,cs,df,d,ds,ef,e,es,ff,f,fs,gf,g,gs,af,a,as,bf,b,bs)

> import qualified Haskore.Music  as Music
> import qualified Haskore.Melody as Melody

> import qualified Data.Accessor.Basic      as Accessor
> import qualified Data.Accessor.Show as AccShow

> type Note = Melody.Note NoteAttributes

> type T    = Melody.T    NoteAttributes

\end{haskelllisting}

%                    | Dynamics String
%                    | Fingering Int

Recall that the \code{Note} constructor contained a field of \code{NoteAttribute}s.
These are values that are attached to notes for the
purpose of notation or musical interpretation.

\begin{haskelllisting}

> data NoteAttributes =
>    NoteAttributes {
>       velocity_ :: Rational, -- intensity of playing between 0 and 1
>       vibrato_  :: (Rational, Rational),
>       tremolo_  :: (Rational, Rational)
>    } deriving (Eq, Ord)
>
> instance Show NoteAttributes where
>    showsPrec =
>       AccShow.showsPrec
>          [AccShow.field "velocity1" velocity1,
>           AccShow.field "vibrato"   vibrato,
>           AccShow.field "tremolo"   tremolo]
>          "na" na
>
> na :: NoteAttributes
> na = NoteAttributes 1 (0,0) (0,0)
>
> velocity1 :: Accessor.T NoteAttributes Rational
> velocity1 =
>    Accessor.fromSetGet (\v nas -> nas{velocity_ = v}) velocity_
>
> vibrato :: Accessor.T NoteAttributes (Rational, Rational)
> vibrato =
>    Accessor.fromSetGet (\v nas -> nas{vibrato_ = v}) vibrato_
>
> tremolo :: Accessor.T NoteAttributes (Rational, Rational)
> tremolo =
>    Accessor.fromSetGet (\v nas -> nas{tremolo_ = v}) tremolo_

\end{haskelllisting}

\begin{haskelllisting}

> fromMelodyNullAttr :: Melody.T () -> T
> fromMelodyNullAttr =
>    Music.mapNote (\(Melody.Note _ p) -> Melody.Note na p)

\end{haskelllisting}

%    Music.mapNote (Accessor.set Melody.noteAttrs na)
