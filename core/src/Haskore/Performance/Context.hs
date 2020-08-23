module Haskore.Performance.Context
   (T,
    setPlayer, setDur, setTranspose, setDynamics,
    getPlayer, getDur, getTranspose, getDynamics,
    Pf.updatePlayer, Pf.updateDur, Pf.updateTranspose, Pf.updateDynamics,
    contextPlayer, contextDur, contextTranspose, contextDynamics, )
   where

import qualified Haskore.Basic.Pitch    as Pitch
-- import qualified Haskore.Music as Music
import qualified Haskore.Performance as Pf
import qualified Haskore.Performance.Player as Player
import Haskore.Performance(Context(..))

-- import qualified Numeric.NonNegative.Class as NonNeg


{- If the Haskell compilers would support mutual depending modules
   the Context data type would be declared here instead of in Performance. -}

type T time dyn note = Pf.Context time dyn note

type SetContext time dyn note a = a -> T time dyn note -> T time dyn note

setPlayer     :: SetContext time dyn note (Player.T time dyn note)
setPlayer     = Pf.updatePlayer . const
setDur        :: SetContext time dyn note time
setDur        = Pf.updateDur . const
setTranspose  :: SetContext time dyn note Pitch.Relative
setTranspose  = Pf.updateTranspose . const
setDynamics   :: SetContext time dyn note dyn
setDynamics   = Pf.updateDynamics . const


type GetContext time dyn note a = T time dyn note -> a

getPlayer     :: GetContext time dyn note (Player.T time dyn note)
getPlayer     = Pf.contextPlayer
getDur        :: GetContext time dyn note time
getDur        = Pf.contextDur
getTranspose  :: GetContext time dyn note Pitch.Relative
getTranspose  = Pf.contextTranspose
getDynamics   :: GetContext time dyn note dyn
getDynamics   = Pf.contextDynamics
