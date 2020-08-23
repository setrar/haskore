module Medium.Controlled where

-- import qualified Medium
-- import qualified Medium.Temporal as Temporal


class C medium where
   control :: (control -> medium control a -> medium control a)

   {- Do actions on each (virtual) constructor, don't recourse. -}
   switchBinary ::
      (a -> b) ->
      (medium control a -> medium control a -> b) ->
      (medium control a -> medium control a -> b) ->
      (control -> medium control a -> b) ->
      (b -> medium control a -> b)
   switchList ::
      (a -> b) ->
      ([medium control a] -> b) ->
      ([medium control a] -> b) ->
      (control -> medium control a -> b) ->
      medium control a -> b


{-
{- A variant of fmap that does not only allow manipulation of primitives
   but also of the compositions.
   Though the structure must be preserved. -}
mapList :: (Medium.Temporal.C b, Medium.C medium) =>
   (a->b) -> ([medium b]->[medium b]) -> ([medium b]->[medium b]) -> medium a -> medium b
mapList f g h = foldList (prim . f) (serial . g) (parallel . h)

mapListFlat :: (Medium.Temporal.C b, Medium.C medium) =>
   (a -> b) -> ([medium a] -> [medium b]) -> ([medium a] -> [medium b]) -> medium a -> medium b
mapListFlat f g h = switchList (prim . f) (serial . g) (parallel . h)
-}


{- This is even more general than mapList -}
foldList :: C medium =>
   (a->b) -> ([b]->b) -> ([b]->b) -> (c->b->b) -> medium c a -> b
foldList f g h k =
   let recourse    = foldList f g h k
       recurseAll = map recourse
   in  switchList f (g . recurseAll) (h . recurseAll) (\c -> k c . recourse)

foldBin :: C medium =>
   (a->b) -> (b->b->b) -> (b->b->b) -> (c->b->b) -> b -> medium c a -> b
foldBin f g h k z =
   let recourse = foldBin f g h k z
       recurseAll op x y = recourse x `op` recourse y
   in  switchBinary f (recurseAll g) (recurseAll h) (\c -> k c . recourse) z
