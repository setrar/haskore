module Medium.Temporal where

import qualified Numeric.NonNegative.Wrapper as NonNeg

type Dur = NonNeg.Rational

class C a where
  dur  :: a -> Dur
  none :: Dur -> a

class Control control where
  controlDur :: control -> Dur -> Dur
  anticontrolDur :: control -> Dur -> Dur
