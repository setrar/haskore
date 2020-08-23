module Medium.LabeledControlled.List where

import qualified Medium.Controlled.List as CtrlMediumList
import qualified Medium.Controlled as CtrlMedium
import qualified Medium
-- import qualified Medium.Temporal as Temporal
-- import Haskore.General.Utility(maximum0)

import Control.Applicative (liftA, )
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(sequenceA))
import qualified Data.Traversable as Traversable

{- |
Medium type with a label
(e.g. the duration of the represented music),
a controller constructor
and direct support for rests.
-}
data T label control content =
   Cons {label :: label,
         structure :: Structure label control content}
   deriving (Show, Eq, Ord {- for use in FiniteMap -})

data Structure label control content =
      Primitive content                          -- ^ primitive content
    | Serial   [T label control content]         -- ^ sequential composition
    | Parallel [T label control content]         -- ^ parallel composition
    | Control  control (T label control content) -- ^ controller
   deriving (Show, Eq, Ord {- for use in FiniteMap -})


class Label label where
   emptyLabel :: label
   -- error "We can not automatically assign a label to primitives created by the generic Medium.primitive method"
   foldLabelSerial   :: [label] -> label
   foldLabelParallel :: [label] -> label


serialLabel, parallelLabel :: Label label =>
   [T label control content] -> T label control content
serialLabel   xs = Cons (foldLabelSerial   (map label xs)) (Serial   xs)
parallelLabel xs = Cons (foldLabelParallel (map label xs)) (Parallel xs)


instance (Label label) => Medium.Construct (T label control) where
   prim = Cons emptyLabel . Primitive

   {- If the operands are also Serials or Parallels
      the lists are joined,
      since most times the operators are used to construct lists.
      This definition works also infinite application of (+:+). -}
   (+:+) x y = serialLabel   (serialToList   x ++ serialToList   y)
   (=:=) x y = parallelLabel (parallelToList x ++ parallelToList y)

   serial1   = serialLabel
   parallel1 = parallelLabel

   serial   = serialLabel
   parallel = parallelLabel



switchList ::
   (label -> b -> c) ->
   (a -> b) ->
   ([T label control a] -> b) ->
   ([T label control a] -> b) ->
   (control -> T label control a -> b) ->
   (T label control a -> c)
switchList lab f g h k (Cons l s) =
   lab l $
   case s of
      Primitive x  -> f x
      Serial   m   -> g m
      Parallel m   -> h m
      Control  c m -> k c m


foldList ::
   (label -> b -> c) ->
   (a -> b) ->
   ([c] -> b) ->
   ([c] -> b) ->
   (control -> c -> b) ->
   (T label control a -> c)
foldList lab f g h k =
   let recourse = foldList lab f g h k
   in  switchList lab f
          (g . map recourse) (h . map recourse) (\c -> k c . recourse)


fromControlledMediumList :: Label label =>
   (a -> (label, b)) -> (control -> T label control b -> label) ->
   CtrlMediumList.T control a -> T label control b
fromControlledMediumList f k =
   CtrlMedium.foldList
      ((\(lab,x) -> Cons lab (Primitive x)) . f)
      serialLabel
      parallelLabel
      (\c x -> Cons (k c x) (Control c x))


mapLabel :: (i -> j) -> (T i control a -> T j control a)
mapLabel f =
   foldList (Cons . f) Primitive Serial Parallel Control

instance Functor (T i control) where
   fmap f = foldList Cons (Primitive . f) Serial Parallel Control
--   fmap = Traversable.fmapDefault

instance Foldable (T i control) where
   foldMap = Traversable.foldMapDefault

instance Traversable (T i control) where
   sequenceA =
      foldList
         (liftA . Cons)
         (liftA Primitive)
         (liftA Serial . sequenceA)
         (liftA Parallel . sequenceA)
         (liftA . Control)

{-
instance (Temporal.C a) => Temporal.C (T a) where
   dur  = Medium.foldList Temporal.dur sum maximum0
   none = Medium.prim . Temporal.none
-}


{- This behaves identical to Medium.Binary,
   if the top most constructor is no serial composition
   it returns a single element list. -}
serialToList, parallelToList :: T label control a -> [T label control a]

serialToList (Cons _ (Serial ns)) = ns
serialToList n                    = [n]

parallelToList (Cons _ (Parallel ns)) = ns
parallelToList n                      = [n]
