
\subsection{Pretty printing Music}

This module aims at formatting (pretty printing) of musical objects with Haskell syntax.
This is particularly useful for converting algorithmically generated music
into Haskell code that can be edited and furtherly developed.

\begin{haskelllisting}

> module Haskore.Process.Format where
>
> import qualified Language.Haskell.Pretty as Pretty
> import qualified Language.Haskell.Syntax as Syntax
> import qualified Language.Haskell.Parser as Parser

> import qualified Haskore.Basic.Duration  as Duration
> import qualified Haskore.Music           as Music
> import qualified Haskore.Melody          as Melody
> import qualified Haskore.Melody.Standard as StdMelody
> import qualified Medium.Controlled as CtrlMedium

> import Medium.Controlled.ContextFreeGrammar as Grammar
> import qualified Haskore.General.Map as Map
> import qualified Data.Ratio as Ratio
> import qualified Data.Char  as Char
> import Data.List(intersperse)

\end{haskelllisting}


Format a grammar as computed with the \module{Medium.Controlled.ContextFreeGrammar}.

\begin{haskelllisting}

> prettyGrammarMedium :: (Show prim, Show control) =>
>    Grammar.T String control prim -> String
> prettyGrammarMedium = prettyGrammar controlGen prim

> prettyGrammarMelody ::
>    Grammar.T String Music.Control (Music.Primitive StdMelody.Note) -> String
> prettyGrammarMelody = prettyGrammar control primMelody

> prettyGrammar ::
>    (Int -> control -> (Int -> ShowS) -> ShowS) ->
>    (Int -> prim -> ShowS) ->
>    Grammar.T String control prim -> String
> prettyGrammar controlSyntax primSyntax g =
>    let text = unlines (map (flip id "" . bind controlSyntax primSyntax) g)
>        Parser.ParseOk (Syntax.HsModule _ _ _ _ code) =
>           Parser.parseModule text
>    in  unlines (map Pretty.prettyPrint code) -- show code

\end{haskelllisting}

Format a \code{Medium} object that contains references to other medium objects.

\begin{haskelllisting}

> bind ::
>    (Int -> control -> (Int -> ShowS) -> ShowS) ->
>    (Int -> prim -> ShowS) ->
>    (String, Grammar.TagMedium String control prim) -> ShowS
> bind controlSyntax primSyntax (key, ms) =
>    showString key . showString " = " . tagMedium 0 controlSyntax primSyntax ms

> tagMedium ::
>    Int ->
>    (Int -> control -> (Int -> ShowS) -> ShowS) ->
>    (Int -> prim -> ShowS) ->
>    Grammar.TagMedium String control prim -> ShowS
> tagMedium prec controlSyntax primSyntax m =
>    let primSyntax' _     (Grammar.Call s) = showString s
>        primSyntax' prec' (Grammar.CallMulti n s) =
>           enclose prec' 0
>              (showString "serial $ replicate " . showsPrec 10 n .
>               showString " " . showString s)
>        primSyntax' prec' (Grammar.Prim p) = primSyntax prec' p
>    in  CtrlMedium.foldList
>           (flip primSyntax')
>           (listFunc "serial")
>           (listFunc "parallel")
>           (flip . flip controlSyntax)
>           m prec

> list :: [Int -> ShowS] -> ShowS
> list = foldr (.) (showString "]") . (showString "[" :) .
>           intersperse (showString ",") . map (flip id 0)

> listFunc :: String -> [Int -> ShowS] -> Int -> ShowS
> listFunc func ps prec =
>    enclose prec 10 (showString func . showString " " . list ps)

> prim :: (Show p) => Int -> p -> ShowS
> prim prec p = enclose prec 10 (showString "prim " . showsPrec 10 p)

> dummySrcLoc :: Syntax.SrcLoc
> dummySrcLoc = Syntax.SrcLoc {Syntax.srcFilename = "",
>                              Syntax.srcLine = 0,
>                              Syntax.srcColumn = 0}

\end{haskelllisting}

Of course we also want to format plain music,
that is music without tags.

\begin{haskelllisting}

> prettyMelody :: StdMelody.T -> String
> prettyMelody m = prettyExp (melody 0 m "")

> prettyExp :: String -> String
> prettyExp text =
>    let Parser.ParseOk (Syntax.HsModule _ _ _ _
>           [Syntax.HsPatBind _ _ (Syntax.HsUnGuardedRhs code) _]) =
>              Parser.parseModule ("dummy = "++text)
>    in  Pretty.prettyPrint code

\end{haskelllisting}

Now we go to define functions that handle
the particular primitives of music.
Note that \code{Control} information
and \code{NoteAttribute}s are printed as atoms.

\begin{haskelllisting}

> melody :: Int -> StdMelody.T -> ShowS
> melody prec m =
>    Music.foldList
>       (flip . flip atom)
>       (flip . flip control)
>       (listFunc "line")
>       (listFunc "chord")
>       m prec

> primMelody :: Int -> Music.Primitive StdMelody.Note -> ShowS
> primMelody prec (Music.Atom d at) = atom prec d at

> atom :: Show attr =>
>    Int -> Duration.T -> Music.Atom (Melody.Note attr) -> ShowS
> atom prec d = maybe (rest prec d) (note prec d)

> note :: Show attr =>
>    Int -> Duration.T -> Melody.Note attr -> ShowS
> note prec d (Melody.Note nas (o,pc)) =
>    enclose prec 10 (showString (map Char.toLower (show pc)) .
>       showString " " . showsPrec 10 o .
>       showString " " . durSyntax id "n" d .
>       showString " " . showsPrec 10 nas)

> rest :: Int -> Duration.T -> ShowS
> rest prec d =
>    durSyntax (\dStr -> enclose prec 10 (showString "rest " . dStr)) "nr" d

> controlGen :: (Show control) => Int -> control -> (Int -> ShowS) -> ShowS
> controlGen prec c m =
>    enclose prec 10
>      (showString "control " . showsPrec 10 c .
>       showString " " . m 10)

> control :: Int -> Music.Control -> (Int -> ShowS) -> ShowS
> control prec c m =
>    let controlSyntax name arg =
>           enclose prec 10
>              (showString name . showString " " . arg . showString " " . m 10)
>    in  case c of
>           Music.Tempo d     -> controlSyntax "changeTempo" (showDur   10 d)
>           Music.Transpose p -> controlSyntax "transpose"   (showsPrec 10 p)
>           Music.Player p    -> controlSyntax "setPlayer"   (showsPrec 10 p)
>           Music.Phrase p    -> controlSyntax "phrase"      (showsPrec 10 p)

\end{haskelllisting}

Note that the call to \code{show} can't be moved
from the \code{controlSyntax} calls in \code{control}
to \code{controlSyntax}
because that provokes a compiler problem, namely

\begin{haskelllisting}

   Mismatched contexts
   When matching the contexts of the signatures for
     controlSyntax :: forall a.
                      (Show a) =>
                      String -> a -> StdMelody.T -> Language.Haskell.Syntax.HsExp
     control :: Music.Primitive -> Language.Haskell.Syntax.HsExp
   The signature contexts in a mutually recursive group should all be identical
   When generalising the type(s) for controlSyntax, control

\end{haskelllisting}

\begin{haskelllisting}

> durSyntax :: (ShowS -> ShowS) -> String -> Duration.T -> ShowS
> durSyntax showRatio suffix d =
>    maybe
>       (showRatio (showDur 10 d))
>       (\s -> showString (s++suffix))
>       (Map.lookup Duration.nameDictionary d)

> showDur :: Int -> Duration.T -> ShowS
> showDur prec =
>    (\d -> enclose prec 7
>         (shows (Ratio.numerator d) .
>          showString "%+" .
>          shows (Ratio.denominator d))) .
>    Duration.toRatio

\end{haskelllisting}

Enclose an expression in parentheses if the inner operator
has at most the precedence of the outer operator.

\begin{haskelllisting}

> enclose :: Int -> Int -> ShowS -> ShowS
> enclose outerPrec innerPrec = showParen (outerPrec >= innerPrec)

\end{haskelllisting}
