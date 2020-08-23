module Haskore.Example.Fractal where

import Prelude hiding (init)
import System.Random (randomRs, mkStdGen)
import Data.Array (Array, (!), listArray, bounds)

import qualified Haskore.Basic.Pitch  as Pitch
import qualified Haskore.Music        as Music
import qualified Haskore.Melody       as Melody
import Haskore.Music((+:+))

import qualified Haskore.Basic.Duration as Dur

type Vector a = [a]
type Matrix a = [Vector a]
type AT     a = Vector a -> Vector a
type IFS    a = Array Int (AT a)

-- First define some general matrix operations.
-- These will facilitate moving to higher dimensions later.

vadd :: Num a => Vector a -> Vector a -> Vector a
vadd = zipWith (+)

vvmult :: Num a => Vector a -> Vector a -> a
vvmult v1 v2 = sum (zipWith (*) v1 v2)

mvmult :: Num a => Matrix a -> Vector a -> Vector a
mvmult m v = map (vvmult v) m

cvmult :: Num a => a -> Vector a -> Vector a
cvmult z = map (z*)

---------------------------------------------------------------------

{- The following simulates the Iterated Function System for the
   Sierpinski Triangle as described in Barnsley's "Desktop Fractal
   Design Handbook". -}

-- First the affine transformations:

w0, w1, w2 :: Fractional a => AT a
w0 v = (cvmult 0.01 ([[50,0],[0,50],[50,0]] `mvmult` v))
       `vadd` [8,8,8]
w1 v = (cvmult 0.01 ([[50,0],[0,50],[50,0]] `mvmult` v))
       `vadd` [30,16,2]
w2 v = (cvmult 0.01 ([[50,0],[0,50],[50,0]] `mvmult` v))
       `vadd` [20,40,30]

init0 :: Num a => Vector a
init0 = [0,0,0]

-- Now we have an Iterated Function System:

ws :: Fractional a => IFS a
ws = let wl = [w0,w1,w2]
     in  listArray (0, length wl - 1) wl

-- And here is the result:

result :: [Vector Rational]
result =
   let ws' = ws -- make it monomorph
       f init r = (ws'!r) init
   in  scanl f init0 (randomRs (bounds ws') (mkStdGen 215))
   -- (read "42" :: StdGen)
         

-- where "randomRs" computes a list of random indices in the range 0-2,
-- which simulates flipping the coin in Barnsley.

--------

mkNote :: [Rational] -> Melody.T ()
mkNote [a,b,c] =
   Music.rest (Dur.fromRatio (b/20)) +:+
   Melody.note (Pitch.fromInt (round a)) (Dur.fromRatio (c/20)) ()
mkNote _ = error "mkNote: Need three components."

{- Of course, a triple would be the better type
   but that would complicate the vector computation. -}

sourceToMusic :: [[Rational]] -> Melody.T ()
sourceToMusic s = Music.chord (map mkNote s)

song :: Melody.T ()
song = Music.transpose (-12) (sourceToMusic (take 128 result))
