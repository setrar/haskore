module Medium where

import qualified Medium.Temporal as Temporal


infixr 7 +:+  {- like multiplication -}
infixr 6 =:=  {- like addition -}


class Construct medium where
   prim :: a -> medium a

   {- for easy compatibility with Haskore 2000 songs
      replace :+: by +:+ and :=: by =:= -}
   (+:+), (=:=) :: medium a -> medium a -> medium a

   serial, parallel :: Temporal.C a => [medium a] -> medium a
   serial1, parallel1 :: [medium a] -> medium a

class Construct medium => C medium where
   {- Do actions on each (virtual) constructor, don't recourse. -}
   switchBinary ::
         (a -> b) -> (medium a -> medium a -> b) -> (medium a -> medium a -> b)
      -> (b -> medium a -> b)
   switchList :: (a -> b) -> ([medium a] -> b) -> ([medium a] -> b)
      -> medium a -> b


{- A variant of fmap that does not only allow manipulation of primitives
   but also of the compositions.
   Though the structure must be preserved. -}
mapList :: (Temporal.C b, Medium.C medium) =>
   (a->b) -> ([medium b]->[medium b]) -> ([medium b]->[medium b]) -> medium a -> medium b
mapList f g h = foldList (prim . f) (serial . g) (parallel . h)

mapListFlat :: (Temporal.C b, Medium.C medium) =>
   (a -> b) -> ([medium a] -> [medium b]) -> ([medium a] -> [medium b]) -> medium a -> medium b
mapListFlat f g h = switchList (prim . f) (serial . g) (parallel . h)


{- This is even more general than mapList -}
foldList :: Medium.C medium => (a->b) -> ([b]->b) -> ([b]->b) -> medium a -> b
foldList f g h =
   let recourse = map (foldList f g h)
   in  switchList f (g . recourse) (h . recourse)

foldBin :: Medium.C medium => (a->b) -> (b->b->b) -> (b->b->b) -> b -> medium a -> b
foldBin f g h z =
   -- foldList f (foldr1 g) (foldr1 h)
   -- this implementation preserves the structure of the binary tree
   let recourse op x y = foldBin f g h z x `op` foldBin f g h z y
   in  switchBinary f (recourse g) (recourse h) z


listMediumFromAny :: (Construct dst, C src, Temporal.C a) => src a -> dst a
listMediumFromAny  =  foldList prim serial parallel

binaryMediumFromAny :: (Construct dst, C src) => dst a -> src a -> dst a
binaryMediumFromAny z  =  foldBin prim (+:+) (=:=) z
