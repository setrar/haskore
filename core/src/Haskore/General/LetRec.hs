module Haskore.General.LetRec where

import Data.Tuple.HT (mapFst, mapSnd, )
import qualified Data.Map as Map


data Expr =
    Const String
  | Append Expr Expr
  | Var Var
   deriving (Show)

type Var = Int
type Count = Int


knot ::
   (Count, ([Expr], (Expr, a)) -> ([Expr], (Expr, b))) ->
   (Count, ([Expr], a) -> ([Expr], b))
knot (count, f) =
   (succ count,
    \(equs0, a) ->
       let (equs1, (rhs, b)) = f (equs0, (Var count, a))
       in  (rhs : equs1, b))

beginKnot ::
   (a -> b) ->
   (Count, ([Expr], a) -> ([Expr], b))
beginKnot f =
   (0, mapSnd f)

endKnot ::
   (Count, ([Expr], a) -> ([Expr], b)) ->
   (a -> ([Expr], b))
endKnot f a = snd f ([], a)


exampleLet ::
   (Expr, (Expr, ())) ->
   (Expr, (Expr, Expr))
exampleLet (a,(b,())) =
   (Append (Const "ab") b,
    (Append (Const "c") a,
     a))

{-
Maybe we can replace manual repeated application of 'knot'
by a type class method.
-}
exampleEqus :: ([Expr], Expr)
exampleEqus =
   mapFst reverse $
   endKnot (knot (knot (beginKnot exampleLet))) ()

exampleResult :: String
exampleResult =
   let mapExpr = Map.fromAscList $ zip [0..] $ fst exampleEqus
       resolve x =
          case x of
             Const str -> str
             Append a b ->
                resolve a ++ resolve b
             Var n -> Map.findWithDefault
                (error $ "unknown variable id " ++ show n ++ " - bug in 'knot'?")
                n mapRes
       mapRes = fmap resolve mapExpr
   in  resolve $ snd exampleEqus
