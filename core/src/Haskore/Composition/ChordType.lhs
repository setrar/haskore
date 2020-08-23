
\begin{haskelllisting}

> module Haskore.Composition.ChordType
>           (T, toChord, parse, fromString, toString) where
>
> import qualified Haskore.Composition.Chord  as Chord
> import qualified Haskore.Basic.Pitch as Pitch
> import qualified Text.ParserCombinators.ReadP as ReadP
> import           Text.ParserCombinators.ReadP (ReadP)
> import qualified Data.Array as Array
> import           Data.Array(Array, Ix, (!), )
> import           Data.Tuple.HT (mapSnd, )
> import           Control.Monad (liftM2, liftM3, )

\end{haskelllisting}

% http://www.geocities.com/melatefet/chordsr.htm

\begin{haskelllisting}

> data T = Cons Third Fourth [Fifth]
>    deriving (Show, Eq)
>
> toChord :: T -> Chord.T
> toChord (Cons third fourth fifth) =
>    scanl (\p (rel,rp) -> if rel then p+rp else rp) 0
>          (foldl (flip fifthToSteps)
>             (fourthToSteps third fourth
>                (thirdToSteps third)) fifth)

> thirdToSteps :: Third -> [Pitch.Relative]
> thirdToSteps third =
>    case third of
>      ThirdMajor -> [4,3]
>      ThirdAugmentedFifth -> [4,4]
>      ThirdDiminishedFifth -> [4,2]
>      ThirdMinor -> [3,4]
>      ThirdMinorAugmentedFifth -> [3,5]
>      ThirdMinorDiminishedFifth -> [3,3]
>      ThirdDiminished -> [3,3]
>      ThirdSustained2 -> [2,5]
>      ThirdSustained4 -> [5,2]
>      ThirdDiminishedAugmented -> [3,3,3]

> absP, relP :: Pitch.Relative -> (Bool,Pitch.Relative)
> absP = (,) False
> relP = (,) True

> fourthToSteps ::
>    Third -> Fourth -> [Pitch.Relative] -> [(Bool,Pitch.Relative)]
> -- (True,p) - p relative pitch to the previous note in the chord
> -- (False,p) - p absolute pitch
> fourthToSteps third fourth ps =
>    let bps = map relP ps
>    in  case fourth of
>          FourthNone -> bps
>          FourthSecond -> bps++[absP 2]
>          FourthSixth -> bps++[absP 9]
>          FourthSixthNineth -> bps++[absP 9, relP 5]
>          FourthSeventh ->
>            if third==ThirdDiminished
>              then bps++[relP 3]
>              else bps++[absP 10]
>          FourthMajorSeventh -> bps++[absP 11]
>          FourthNineth -> bps++[relP 10, absP 2]
>          FourthMajorNineth -> bps++[absP 11, relP 3]
>          FourthEleventh -> [absP 7, relP 3, relP 4, absP 5]
>          FourthThirteenth -> [absP (head ps), relP 5, absP 2, absP 10]

> updateNode :: Int -> a -> (a -> a) -> [a] -> [a]
> updateNode n deflt f xs =
>    let (x0,x1) = splitAt n xs
>    in  x0 ++ case x1 of
>                [] -> [f deflt]
>                (y:ys) -> f y : ys

> incPitch :: Int -> Pitch.Relative -> Pitch.Relative ->
>       [(Bool,Pitch.Relative)] -> [(Bool,Pitch.Relative)]
> incPitch n deflt inc =
>    updateNode n (False,deflt) (mapSnd (inc+))

> fifthToSteps :: Fifth -> [(Bool,Pitch.Relative)] -> [(Bool,Pitch.Relative)]
> fifthToSteps fifth =
>    case fifth of
>      FifthAugmentedThird    -> incPitch 0 undefined 1 .
>                                incPitch 1 undefined (-1)
>      FifthDiminishedFifth   -> incPitch 1 undefined (-1)
>      FifthAugmentedFifth    -> incPitch 1 undefined 1
>      FifthMajorSeventh      -> incPitch 2 10 1
>      FifthMinorNineth       -> incPitch 3 14 (-1)
>      FifthMajorNineth       -> incPitch 3 14 1
>      FifthAugmentedEleventh -> incPitch 3 17 1

\end{haskelllisting}

\begin{haskelllisting}

> data Third =
>      ThirdMajor
>    | ThirdAugmentedFifth
>    | ThirdDiminishedFifth
>    | ThirdMinor
>    | ThirdMinorAugmentedFifth
>    | ThirdMinorDiminishedFifth
>    | ThirdDiminished
>    | ThirdSustained2
>    | ThirdSustained4
>    | ThirdDiminishedAugmented
>      deriving (Show, Eq, Ord, Ix)
>
> data Fourth =
>      FourthNone
>    | FourthSecond
>    | FourthSixth
>    | FourthSixthNineth
>    | FourthSeventh
>    | FourthMajorSeventh
>    | FourthNineth
>    | FourthMajorNineth
>    | FourthEleventh
>    | FourthThirteenth
>      deriving (Show, Eq, Ord, Ix)
> 
> data Fifth =
>      FifthAugmentedThird
>    | FifthDiminishedFifth
>    | FifthAugmentedFifth
>    | FifthMajorSeventh
>    | FifthMinorNineth
>    | FifthMajorNineth
>    | FifthAugmentedEleventh
>      deriving (Show, Eq, Ord, Ix)
>
> toString :: T -> String
> toString (Cons third fourth fifthList) =
>    thirdsArray!third ++
>    fourthsArray!fourth ++
>    concatMap (fifthsArray!) fifthList
>
> intervalToArray :: (Ix a) => [(a,[String])] -> Array a String
> intervalToArray xs =
>    Array.array (fst (head xs), fst (last xs))
>                (map (mapSnd head) xs)
>
> thirdsArray :: Array Third String
> thirdsArray = intervalToArray thirds
>
> fourthsArray :: Array Fourth String
> fourthsArray = intervalToArray fourths
>
> fifthsArray :: Array Fifth String
> fifthsArray = intervalToArray fifths
>
> fromString :: String -> T
> fromString =
>    fst . head . filter (null . snd) . ReadP.readP_to_S parse
>
> -- copy of GHC-6.4's ReadP.many function
> readPmany :: ReadP a -> ReadP [a]
> readPmany p = return [] ReadP.+++ liftM2 (:) p (readPmany p)
>
> parse :: ReadP T
> parse =
>    liftM3 Cons
>           (parseInterval thirds)
>           (parseInterval fourths)
>           (readPmany (parseInterval fifths))
>
> parseInterval :: [(a,[String])] -> ReadP a
> parseInterval =
>    ReadP.choice . map (uncurry parseIntervalAlternatives)
>
> parseIntervalAlternatives :: a -> [String] -> ReadP a
> parseIntervalAlternatives x sym =
>    ReadP.choice (map ReadP.string sym) >> return x
>
> thirds :: [(Third,[String])]
> thirds = [
>   (ThirdMajor, ["", "maj"]),
>   (ThirdAugmentedFifth, ["+", "aug"]),
>   (ThirdDiminishedFifth, ["-"]),
>   (ThirdMinor, ["m"]),
>   (ThirdMinorAugmentedFifth, ["m+"]),
>   (ThirdMinorDiminishedFifth, ["m-"]),
>   (ThirdDiminished, ["0", "dim"]),
>   (ThirdSustained2, ["sus2"]),
>   (ThirdSustained4, ["sus4", "4"]),
>   (ThirdDiminishedAugmented, ["0+"])
>  ]

> fourths :: [(Fourth,[String])]
> fourths = [
>   (FourthNone, [""]),
>   (FourthSecond, ["2"]),
>   (FourthSixth, ["6"]),
>   (FourthSixthNineth, ["6/9"]),
>   (FourthSeventh, ["7"]),
>   (FourthMajorSeventh, ["M7", "Ma7"]),  -- "maj7" collides with "maj"++"7"
>   (FourthNineth, ["9"]),
>   (FourthMajorNineth, ["M9"]),
>   (FourthEleventh, ["11"]),
>   (FourthThirteenth, ["13"])
>  ]

> fifths :: [(Fifth,[String])]
> fifths = [
>   (FifthAugmentedThird, ["3+"]),
>   (FifthDiminishedFifth, ["-5", "5-"]),
>   (FifthAugmentedFifth, ["+5", "5+", "-6", "6-"]),
>   (FifthMajorSeventh, ["7+"]),
>   (FifthMinorNineth, ["-9"]),
>   (FifthMajorNineth, ["+9"]),
>   (FifthAugmentedEleventh, ["+11"])
>  ]

\end{haskelllisting}
