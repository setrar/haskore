{- |
Import Music from text printed by OctaMED.
It will be certainly easy to adapt that for other trackers
like SoundTracker, NoiseTracker, DigiBooster, FastTracker.

Take care that you use B not H note name.
-}
module Haskore.Interface.MED.Text where

import qualified Haskore.Basic.Pitch as Pitch
import qualified Haskore.Melody      as Melody

import qualified Haskore.Process.Format as Fmt

import qualified Text.ParserCombinators.Parsec.Combinator as ParseComb
import qualified Text.ParserCombinators.Parsec.Char as Parse
import Text.ParserCombinators.Parsec.Char (CharParser)
import Text.ParserCombinators.Parsec.Prim ((<|>), parse)

import Haskore.General.Utility (splitBy)
import Haskore.Basic.Duration((%+))
import Data.Char (ord)
import Data.Maybe (isJust)
import qualified Data.List as List
import Control.Monad (liftM2, )


{- | should be moved to Utility -}
sieve :: Int -> [a] -> [a]
sieve k = map head . takeWhile (not . null) . iterate (drop k)

{- | should be moved to Utility -}
sliceHoriz :: Int -> [a] -> [[a]]
sliceHoriz n =
   map (sieve n) . take n . iterate (drop 1)

{- | should be moved to Utility -}
sliceVert :: Int -> [a] -> [[a]]
sliceVert n =
   map (take n) . takeWhile (not . null) . iterate (drop n)

type Instrument = Int


splitBlocks ::
      [String]
   -> [[String]]
splitBlocks =
   map (takeWhile (not . List.isPrefixOf "\f") . tail) .
   filter ((replicate 33 '=' ==) . head) .
   List.init .
   List.tails


cellToNote :: String -> (Maybe (Pitch.T,Instrument), String)
cellToNote =
   either (error . show) id . parse parseCell "cell"

parseDigit :: CharParser () Int
parseDigit =
   fmap (\c -> ord c - ord '0') Parse.digit

parseNote :: CharParser () (Maybe (Pitch.T,Instrument))
parseNote =
   (do pitchClass <-
          liftM2 (\ bc m -> read(bc:m))
             (Parse.satisfy (\p -> 'A' <= p && p <= 'G'))
             ((Parse.char '-' >> return "") <|>
              (Parse.char '#' >> return "s"))
       octave <- parseDigit
       instr <-
          liftM2 (\ instrH instrL -> instrH*32+instrL)
             ((Parse.char ' ' >> return 0) <|>
              parseDigit)
             (parseDigit <|>
              (fmap (\c -> ord c - ord 'A' + 10)
                  (Parse.satisfy (\p -> 'A' <= p && p <= 'V'))))
       return (Just ((octave,pitchClass), instr)))
   <|>
   (do _ <- Parse.char '-'
       _ <- ParseComb.count 4 ParseComb.anyToken
       return Nothing)

parseCell :: CharParser () (Maybe (Pitch.T,Instrument), String)
parseCell =
   liftM2 (,) parseNote (ParseComb.count 4 ParseComb.anyToken)


columnToNotes ::
     [String]
  -> ([String], [(Pitch.T, Instrument, [String])])
columnToNotes cells =
   let notes = splitBy (isJust . fst) . map cellToNote $ cells
       procNote ((Just (pitch,instr), cmd) : rest) =
          (pitch, instr, cmd : map snd rest)
       procNote _ = error "each note must start with Just"
   in  case notes of
          pause@((Nothing, _) : _) : rest ->
              (map snd pause, map procNote rest)
          _ -> ([], map procNote notes)

{- |
Convert a block of a song to a list of notes.
-}
linesToNotes ::
     [String]   {- ^ lines of a block -}
  -> [([String], [(Pitch.T, Instrument, [String])])]
linesToNotes =
   map columnToNotes . List.transpose . map (sliceVert 10 . drop 4)




columnToSimpleSerial ::
     Integer
  -> ([String], [(Pitch.T, Instrument, [String])])
  -> ShowS
columnToSimpleSerial whole (rest, melody) =
   (if null rest
      then id
      else Fmt.rest 5 (List.genericLength rest %+ whole) . showString " : ") .
   foldr (.)
      (showString "[]")
      (map
         (\(pitch,_instr,cmds) ->
            Fmt.note 5
               (List.genericLength cmds %+ whole)
               (Melody.Note () pitch) .
            showString " : ")
         melody)

{-
mapM print . map (map (($"") . columnToSimpleSerial 16) . linesToNotes) . splitBlocks . lines =<< readFile "/data2/AmigaEnvironment/Partitions/Data/Songs/Meine/Air.1.txt"
-}


{-
Convert a block of a song to Music.

blockToMusic ::
     Int      {- ^ length of a whole note -}
  -> String   {- ^ textual representation of a block -}
  -> [[(Pitch.T, Instrument, [String])]]
blockToMusic whole text =
-}
