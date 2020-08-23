% from AutoTrack by Stefan Ratschan

\section{Chord Symbols}

\begin{haskelllisting}

> module Haskore.Interface.AutoTrack.ChordSymbol
>           (T(Cons, root, chordType),
>            toChord,
>            toString, parse) where
> import qualified Haskore.Interface.AutoTrack.Transposeable as Transposeable
> import qualified Haskore.Basic.Pitch  as Pitch
> -- import qualified Haskore.Basic.Scale  as Scale
> import qualified Haskore.Composition.ChordType as ChordType
> import qualified Text.ParserCombinators.ReadP as ReadP
> import           Text.ParserCombinators.ReadP (ReadP)
> import           Data.Tuple.HT (mapSnd, )

\end{haskelllisting}

A chord symbol consists of its root, its bass note, and the description of the type of
chord. The chord type description is currently in free (string) form and only used by some
very experimental code.

\begin{haskelllisting}

> data T = Cons { root      :: Pitch.Class,
>                 bassnote  :: Pitch.Class,
>                 chordType :: ChordType.T } deriving Eq

\end{haskelllisting}

Now we define input and output of chord symbols. Note that we denote sharp and
flat root notes by '\#' and 'b' respectively, instead of 's' and 'f' as in
Haskore.

\begin{haskelllisting}

> instance Show T where
>   showsPrec _ ch =
>      ("(ChordSymbol "++) .
>            shows (root ch) . (" "++) .
>            shows (bassnote ch) . (" "++) .
>            shows (chordType ch) . (")"++)

> instance Read T where
>   readsPrec _ = ReadP.readP_to_S parse

> parse :: ReadP T
> parse =
>     do r <- parsePitch
>        t <- ChordType.parse
>        b <- return r ReadP.+++
>                (ReadP.char '/' >> parsePitch)
>        return (Cons r b t)

> parsePitch :: ReadP Pitch.Class
> parsePitch = ReadP.readS_to_P readSPitch

> readSPitch :: ReadS Pitch.Class
> readSPitch (p:'#':r) = continueReadS r (p:"s")
> readSPitch (p:'b':r) = continueReadS r (p:"f")
> readSPitch (p:r)     = continueReadS r [p]
> readSPitch "" = [] -- error "readSPitch: empty string"

> continueReadS :: (Read a) => String -> ReadS a
> continueReadS r p = map (mapSnd (++r)) (reads p)

\end{haskelllisting}

We also can transpose chord symbols.

\begin{haskelllisting}

> instance Transposeable.C T where
>   transpose i c = Cons (Transposeable.transpose i (root c))
>                        (Transposeable.transpose i (bassnote c))
>                        (chordType c)

\end{haskelllisting}

Now we are going to determine the according scale for various chords. Not that such
``default scales'' exist only for some few chords. We plan to implement a
detailed scale analyzer for chord charts (see section~\ref{sec:charts}) in the
future.

\begin{haskelllisting}

> {-
> toScale :: T -> Scale.T
> toScale (Cons {root=r, chordType=ct}) =
>    (case ct of
>       Type ThirdMajor FourthNone [] -> Scale.ionian
>       Type ThirdMinor FourthNone [] -> Scale.dorian
>       _ -> error ("ChordSymbol.toScale: unknown chord type " ++ show ct)) r
> -}
>
> toChord :: T -> [Pitch.T]
> toChord (Cons {root=r, chordType=ct}) =
>    map (flip Pitch.transpose (0,r)) (ChordType.toChord ct)
>
> toString :: T -> String
> toString chord =
>    let rp = root     chord
>        bp = bassnote chord
>    in  Pitch.classFormat rp
>           (ChordType.toString (chordType chord))
>          ++ if rp == bp then "" else "/"++Pitch.classFormat bp ""

\end{haskelllisting}
