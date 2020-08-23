module Medium.Plain.List where

import qualified Medium
import qualified Medium.Temporal as Temporal

import Haskore.General.Utility(maximum0)

import Control.Applicative (liftA, )
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(sequenceA))
import qualified Data.Traversable as Traversable


data T a = Primitive a
         | Serial   [T a]  -- sequential composition
         | Parallel [T a]  -- parallel composition
   deriving (Show, Eq, Ord {- for use in FiniteMap -})

instance Medium.Construct T where
   prim = Primitive

   {- If the operands are also Serials or Parallels
      the lists are joined,
      since most times the operators are used to construct lists.
      This definition works also for infinite application of (+:+). -}
   (+:+) x y = Serial   (serialToList   x ++ serialToList   y)
   (=:=) x y = Parallel (parallelToList x ++ parallelToList y)


   serial   = Serial
   parallel = Parallel

   serial1   = Serial
   parallel1 = Parallel


instance Medium.C T where
   switchBinary f _ _ _ (Primitive     x)      = f x
   switchBinary _ g _ _ (Serial   (m:ms)) = g m (Serial   ms)
   switchBinary _ _ h _ (Parallel (m:ms)) = h m (Parallel ms)
   switchBinary _ _ _ z _ = z

   switchList f _ _ (Primitive     x) = f x
   switchList _ g _ (Serial   m) = g m
   switchList _ _ h (Parallel m) = h m


instance Functor T where
   fmap f = Medium.foldList (Primitive . f) Serial Parallel
--   fmap = Traversable.fmapDefault

instance Foldable T where
   foldMap = Traversable.foldMapDefault

instance Traversable T where
   sequenceA =
      Medium.foldList
         (liftA Primitive)
         (liftA Serial . sequenceA)
         (liftA Parallel . sequenceA)

instance (Temporal.C a) => Temporal.C (T a) where
   dur  = Medium.foldList Temporal.dur sum maximum0
   none = Medium.prim . Temporal.none


{- This behaves identical to Medium.Plain.Binary,
   if the top most constructor is no serial composition
   it returns a single element list. -}
serialToList, parallelToList :: T a -> [T a]

serialToList (Serial ns) = ns
serialToList n           = [n]

parallelToList (Parallel ns) = ns
parallelToList n             = [n]
