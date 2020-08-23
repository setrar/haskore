
module Haskore.Interface.Signal.InstrumentMap where

import qualified Synthesizer.Plain.Signal as Sig

import Data.Maybe(fromMaybe)
import qualified Data.List as List



type InstrumentTable time v instr =
   [(instr, Instrument time v)]

type DrumTable time v instr =
   [(instr, Drum time v)]

type ToInstrument time v instr =
   instr -> Instrument time v

type ToDrum time v instr =
   instr -> Drum time v

type Instrument time v = time -> time -> Sig.T v
type Drum       time v = time -> Sig.T v


lookup :: Eq instr => [(instr, a)] -> instr -> a
lookup ys x =
   fromMaybe (error "Signal.InstrumentMap.lookup: Instrument unknown")
             (List.lookup x ys)
