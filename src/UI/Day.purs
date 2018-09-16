module UI.Day
  ( module D
  , combine
  , liftLeft
  , liftRight
  , liftUILeft
  , liftUIRight
  ) where

import Prelude

import Control.Comonad (class Comonad)
import Control.Monad.Transition.Trans (TransitionT(..), runTransitionT)
import Control.Comonad.Day (lowerLeft, lowerRight) as D
import Data.Functor.Day (Day, day, runDay) as D
import UI (Component, UI, liftUI)

combine
  :: forall w1 w2 m f a
   . Functor m
  => Comonad w1
  => Comonad w2
  => Component m w1 f a
  -> Component m w2 f a
  -> (UI m (D.Day w1 w2) (f a) -> UI m (D.Day w1 w2) (f a) -> UI m (D.Day w1 w2) (f a))
  -> Component m (D.Day w1 w2) f a
combine c1 c2 append = D.day build c1 c2
  where
    build ui1 ui2 = append (liftUI D.lowerLeft ui1) (liftUI D.lowerRight ui2)

liftLeft :: forall w1 w2 m. Functor w1 => Comonad w2 => TransitionT w1 m ~> TransitionT (D.Day w1 w2) m
liftLeft t = TransitionT (runTransitionT t <<< D.lowerLeft)

liftRight :: forall w1 w2 m. Functor w2 => Comonad w1 => TransitionT w2 m ~> TransitionT (D.Day w1 w2) m
liftRight t = TransitionT (runTransitionT t <<< D.lowerRight)

liftUILeft :: forall w1 w2 m. Comonad w1 => Comonad w2 => Functor m => UI m w1 ~> UI m (D.Day w1 w2)
liftUILeft = liftUI D.lowerLeft

liftUIRight :: forall w1 w2 m. Comonad w1 => Comonad w2 => Functor m => UI m w2 ~> UI m (D.Day w1 w2)
liftUIRight = liftUI D.lowerRight
