module Control.Comonad.Day where

import Prelude

import Control.Comonad (class Comonad, extract)
import Data.Functor.Day (Day, runDay)

lowerLeft :: forall w1 w2. Functor w1 => Comonad w2 => Day w1 w2 ~> w1
lowerLeft = runDay (\f w s -> map (_ `f` extract s) w)

lowerRight :: forall w1 w2. Comonad w1 => Functor w2 => Day w1 w2 ~> w2
lowerRight = runDay (\f w s -> map (f (extract w)) s)
