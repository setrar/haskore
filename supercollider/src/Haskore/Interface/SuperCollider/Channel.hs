{- |
Bookkeeping of SuperCollider channels
-}
module Haskore.Interface.SuperCollider.Channel where

-- import qualified Sound.OpenSoundControl.Transport.Monad as Trans

import qualified Sound.SC3.UGen.Bindings.DB as SCIO
import qualified Sound.SC3.UGen.Rate as SCRate
import Sound.SC3.UGen.Type (UGen, constant, )


type Channel = Int
type NumberChannels = Int


{- |
We reserve the first two channels for the stereo output.
We must not use them for instruments.
This is too unflexible and will certainly change in the future.
-}
least :: Channel
least = 2


data T m =
   Cons {
       next  :: NumberChannels -> m Channel,
       reset :: m ()
--       liftTransport :: Trans.IO t a -> m a
   }




readUGen :: NumberChannels -> Channel -> UGen
readUGen numChan chan =
   SCIO.in' numChan SCRate.AR (constant chan)

writeUGen :: Channel -> UGen -> UGen
writeUGen chan =
   SCIO.out (constant chan)
