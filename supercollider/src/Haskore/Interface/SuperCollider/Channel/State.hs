{- |
An instance of 'Haskore.Interface.SuperCollider.Channel.T'
using a 'StateT' monad transformer.
-}
module Haskore.Interface.SuperCollider.Channel.State where

import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.State (StateT)

import qualified Haskore.Interface.SuperCollider.Channel as ChannelMng
import Haskore.Interface.SuperCollider.Channel
          (Channel, NumberChannels)


manager :: Monad m => ChannelMng.T (StateT Channel m)
manager = ChannelMng.Cons next reset


next :: Monad m =>
   NumberChannels -> StateT Channel m Channel
next inc =
   do num <- State.get
      State.modify (inc+)
      return num

reset :: Monad m =>
   StateT Channel m ()
reset =
   State.put ChannelMng.least
