module Medium.Controlled.List where

import qualified Medium.Controlled as CtrlMedium
import qualified Medium.Plain.List as ListMedium
import qualified Medium
import qualified Medium.Temporal as Temporal
import Haskore.General.Utility(maximum0)

import Control.Applicative (liftA, )
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(sequenceA))
import qualified Data.Traversable as Traversable

{- |
Medium type with a controller constructor.
-}
data T control content =
      Primitive content                    -- ^ primitive content
    | Serial   [T control content]         -- ^ sequential composition
    | Parallel [T control content]         -- ^ parallel composition
    | Control  control (T control content) -- ^ controller
   deriving (Show, Eq, Ord {- for use in FiniteMap -})


instance Medium.Construct (T control) where
   prim = Primitive

   (+:+) x y = serial   (serialToList   x ++ serialToList   y)
   (=:=) x y = parallel (parallelToList x ++ parallelToList y)


   serial   = serial
   parallel = parallel

   serial1   = serial
   parallel1 = parallel



instance CtrlMedium.C T where
   control = Control

   switchBinary f _ _ _ _ (Primitive x)     = f x
   switchBinary _ g _ _ _ (Serial   (m:ms)) = g m (Serial   ms)
   switchBinary _ _ h _ _ (Parallel (m:ms)) = h m (Parallel ms)
   switchBinary _ _ _ k _ (Control  c m)    = k c m
   switchBinary _ _ _ _ z _ = z

   switchList f _ _ _ (Primitive x)  = f x
   switchList _ g _ _ (Serial   m)   = g m
   switchList _ _ h _ (Parallel m)   = h m
   switchList _ _ _ k (Control  c m) = k c m


instance Functor (T control) where
   fmap f = CtrlMedium.foldList (Primitive . f) Serial Parallel Control
--   fmap = Traversable.fmapDefault

instance Foldable (T control) where
   foldMap = Traversable.foldMapDefault

instance Traversable (T control) where
   sequenceA =
      CtrlMedium.foldList
         (liftA Primitive)
         (liftA Serial . sequenceA)
         (liftA Parallel . sequenceA)
         (liftA . Control)

instance (Temporal.C a, Temporal.Control control) =>
      Temporal.C (T control a) where
   dur  = CtrlMedium.foldList Temporal.dur sum maximum0 Temporal.controlDur
   none = Primitive . Temporal.none


{-
This behaves identical to Medium.Binary,
if the top most constructor is no serial composition
it returns a single element list.
-}
serialToList, parallelToList :: T control a -> [T control a]

serialToList (Serial ns) = ns
serialToList n           = [n]

parallelToList (Parallel ns) = ns
parallelToList n             = [n]


prim :: a -> T control a
prim = Primitive

serial, parallel :: [T control a] -> T control a
serial   = Serial
parallel = Parallel



fromMedium :: (Medium.C src) => src a -> T control a
fromMedium  =  Medium.foldList Primitive Serial Parallel

toMediumList :: T control a -> ListMedium.T a
toMediumList  =
   CtrlMedium.foldList ListMedium.Primitive
      ListMedium.Serial ListMedium.Parallel (flip const)



{- A variant of fmap that does not only allow manipulation of primitives
   but also of the compositions.
   Though the structure must be preserved. -}
mapList ::
   (a -> b) ->
   ([T control b] -> [T control b]) ->
   ([T control b] -> [T control b]) ->
   (control -> T control b -> T control b) ->
   T control a -> T control b
mapList f g h k =
   CtrlMedium.foldList (Primitive . f) (Serial . g) (Parallel . h) (\c -> Control c . k c)

mapListFlat ::
   (a -> b) ->
   ([T control a] -> [T control b]) ->
   ([T control a] -> [T control b]) ->
   (control -> T control a -> T control b) ->
   T control a -> T control b
mapListFlat f g h k =
   CtrlMedium.switchList (Primitive . f) (Serial . g) (Parallel . h) (\c -> Control c . k c)

mapControl ::
   (c0 -> c1) -> T c0 a -> T c1 a
mapControl f =
   CtrlMedium.foldList
      Primitive Serial Parallel (Control . f)
