module Medium.Plain.Binary where

import Medium ((+:+), (=:=))

import qualified Medium
import qualified Medium.Temporal as Temporal

import Control.Applicative (liftA, liftA2, )
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(sequenceA))
import qualified Data.Traversable as Traversable

infixr 7 :+:  {- like multiplication -}
infixr 6 :=:  {- like addition -}

data T a = Primitive a
         | T a :+: T a  -- sequential composition
         | T a :=: T a  -- parallel composition
   deriving (Show, Eq, Ord {- for use in FiniteMap -})

instance Medium.Construct T where
   prim = Primitive

   (+:+) = (:+:)
   (=:=) = (:=:)

   serial [] = Primitive (Temporal.none 0)
   serial m  = foldr1 (+:+) m

   parallel [] = Primitive (Temporal.none 0)
   parallel m  = foldr1 (=:=) m

   serial1   = foldr1 (+:+)
   parallel1 = foldr1 (=:=)


instance Medium.C T where
   switchBinary f _ _ _ (Primitive  x) = f x
   switchBinary _ g _ _ (m0:+:m1) = g m0 m1
   switchBinary _ _ h _ (m0:=:m1) = h m0 m1


   switchList f _ _ (Primitive    x) = f x
   switchList _ g _ m@(_ :+: _) = g (serialS   m [])
   switchList _ _ h m@(_ :=: _) = h (parallelS m [])


errorNone :: a
errorNone = error "Program bug: This data structure does not contain empty things."


instance Functor T where
   fmap f = Medium.foldBin (Primitive . f) (:+:) (:=:) errorNone
--   fmap = Traversable.fmapDefault

instance Foldable T where
   foldMap = Traversable.foldMapDefault

instance Traversable T where
   sequenceA =
      Medium.foldBin
         (liftA Primitive)
         (liftA2 (:+:))
         (liftA2 (:=:))
         errorNone


instance Temporal.C a => Temporal.C (T a) where
   dur  = Medium.foldBin Temporal.dur (+) max errorNone
   none = Medium.prim . Temporal.none


serialS, parallelS :: T a -> [T a] -> [T a]

serialS (m0 :+: m1) = serialS m0 . serialS m1
serialS  m0         = (m0 :)

parallelS (m0 :=: m1) = parallelS m0 . parallelS m1
parallelS  m0         = (m0 :)
