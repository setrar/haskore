{- |
Create chord patterns with controlable level of details.
-}
module Haskore.Example.Detail where

import qualified Haskore.Basic.Pitch as Pitch
import Haskore.Basic.Pitch (Class(..))
import qualified Haskore.Melody as Melody
import qualified Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music             as Music

import qualified System.Random as Random

import System.Random (RandomGen, randomR, mkStdGen, )
import Control.Monad.Trans.State (State, state, evalState, )

import Data.Maybe.HT (toMaybe, )
import qualified Data.List as List



levels :: [[Pitch.T]]
levels =
   ((0,C) : []) :
   ((0,C) : (1,C) : []) :
   ((0,C) : (1,C) : (0,G) : []) :
   ((0,C) : (1,C) : (0,G) : (0,E) : []) :
   ((0,C) : (1,C) : (0,G) : (0,E) : (0,D) : (0,F) : []) :
   []


{-
candidate for Utility

cf. Data.MarkovChain.randomItem
-}
randomItem :: (RandomGen g) => [a] -> State g a
randomItem x = fmap (x!!) (randomRState (0, length x - 1))

{- |
'System.Random.randomR' wrapped in a State monad.
-}
randomRState :: (RandomGen g) => (Int,Int) -> State g Int
randomRState bnds = state (randomR bnds)


merge :: [a] -> [a] -> [a]
merge xs ys =
   concat (zipWith (\x y -> [x,y]) xs ys)



dyadicPattern :: [Pitch.T]
dyadicPattern =
   foldl1 merge $
   zipWith
      (\g level -> flip evalState g (sequence (repeat (randomItem level))))
      (List.unfoldr (Just . Random.split) (mkStdGen 925)) $
   levels


simpleSong :: MidiMusic.T
simpleSong =
   Music.changeTempo 2 $
   Music.take 10 $
   MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano $
   MidiMusic.line $
   List.map (\p -> Melody.note p MidiMusic.sn ()) dyadicPattern



dyadicLevelPattern :: [(Int, Pitch.T)]
dyadicLevelPattern =
   foldl1 merge $
   zipWith3
      (\g i level -> map ((,) i) $
          flip evalState g (sequence (repeat (randomItem level))))
      (List.unfoldr (Just . Random.split) (mkStdGen 925))
      [0..] $
   levels


song :: MidiMusic.T
song =
   Music.changeTempo 2 $
   MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano $
   MidiMusic.line $
   List.map (maybe MidiMusic.snr (\p -> Melody.note p MidiMusic.sn ())) $
   List.zipWith
      (\li (l,p) -> toMaybe (l<=li) p)
      (concatMap (replicate (2 * 2 ^ length levels)) [0 .. length levels]) $
   dyadicLevelPattern

