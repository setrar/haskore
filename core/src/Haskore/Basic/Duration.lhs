\subsubsection{Duration}
\seclabel{duration}

\begin{haskelllisting}

> module Haskore.Basic.Duration where

> import qualified Medium.Temporal as TemporalMedium
> import Data.Ratio((%))

> import qualified Haskore.General.Utility as Utility
> import Haskore.General.Map (Map)
> import qualified Haskore.General.Map as Map

> import qualified Numeric.NonNegative.Wrapper as NonNeg

\end{haskelllisting}

\begin{haskelllisting}

> type T = TemporalMedium.Dur
> type Ratio  = T
> type Offset = Rational

> infixl 7 %+
> (%+) :: Integer -> Integer -> T
> (%+) x y = fromRatio (x%y)

> fromRatio :: Rational -> T
> fromRatio = NonNeg.fromNumberMsg "Duration.fromRatio"

> toRatio :: T -> Rational
> toRatio = NonNeg.toNumber

> toNumber :: Fractional a => T -> a
> toNumber = fromRational . NonNeg.toNumber

> scale :: Ratio -> T -> T
> scale = (*)

> add :: Offset -> T -> T
> add d = NonNeg.fromNumberMsg "Duration.add" . (d+) . toRatio

\end{haskelllisting}

\function{add} may have undefined result.

\begin{haskelllisting}

> divide :: T -> T -> Integer
> divide r1 r2 = Utility.divide (toRatio r1) (toRatio r2)

> divisible :: T -> T -> Bool
> divisible r1 r2 = Utility.divisible (toRatio r1) (toRatio r2)

> gcd :: T -> T -> T
> gcd r1 r2 = fromRatio (Utility.gcdDur (toRatio r1) (toRatio r2))

\end{haskelllisting}

\begin{haskelllisting}

> dotted, doubleDotted :: T -> T
> dotted       = ((3%+2) *)
> doubleDotted = ((7%+4) *)
>
> bn, wn, hn, qn, en, sn, tn, sfn    :: T
> dwn, dhn, dqn, den, dsn, dtn       :: T
> ddhn, ddqn, dden                   :: T
>
> bn   = 2       -- brevis
> wn   = 1       -- whole note
> hn   = 1%+ 2    -- half note
> qn   = 1%+ 4    -- quarter note
> en   = 1%+ 8    -- eight note
> sn   = 1%+16    -- sixteenth note
> tn   = 1%+32    -- thirty-second note
> sfn  = 1%+64    -- sixty-fourth note
>
> dwn  = dotted wn    -- dotted whole note
> dhn  = dotted hn    -- dotted half note
> dqn  = dotted qn    -- dotted quarter note
> den  = dotted en    -- dotted eighth note
> dsn  = dotted sn    -- dotted sixteenth note
> dtn  = dotted tn    -- dotted thirty-second note
>
> ddhn = doubleDotted hn  -- double-dotted half note
> ddqn = doubleDotted qn  -- double-dotted quarter note
> dden = doubleDotted en  -- double-dotted eighth note

\end{haskelllisting}


\begin{haskelllisting}

> nameDictionary :: Map T String
> nameDictionary =
>    let names  = "b" : "w" :  "h" :  "q" :  "e" :  "s" :  "t" :  "sf" : []
>        durs   = zip (iterate (/2) 2) names
>        ddurs  = map (\(d,s) -> (dotted       d, "d" ++s)) durs
>        dddurs = map (\(d,s) -> (doubleDotted d, "dd"++s)) durs
>    in  Map.fromList $
>           durs ++
>           take 6 (drop 1 ddurs) ++
>           take 3 (drop 2 dddurs)

> {- |
> Converts @1%4@ to @\"qn\"@ and so on.
> -}
> toString :: T -> String
> toString dur =
>    maybe
>       ("(" ++ show dur ++ ")")
>       (++"n")
>       (Map.lookup nameDictionary dur)

\end{haskelllisting}


Check proper formatting.

\begin{haskelllisting}

> propToString :: Bool
> propToString =
>    all (\(dur,name) -> toString dur == name) $
>      (bn, "bn") : (wn, "wn") : (hn, "hn") : (qn, "qn") :
>      (en, "en") : (sn, "sn") : (tn, "tn") : (sfn, "sfn") :
>      (dwn, "dwn") : (dhn, "dhn") : (dqn, "dqn") :
>      (den, "den") : (dsn, "dsn") : (dtn, "dtn") :
>      (ddhn, "ddhn") : (ddqn, "ddqn") : (dden, "dden") : []

\end{haskelllisting}
