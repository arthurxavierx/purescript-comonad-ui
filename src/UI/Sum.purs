module UI.Sum
  ( module S
  , combine
  , liftLeft
  , liftRight
  , liftUILeft
  , liftUIRight
  , moveLeft
  , moveRight
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Sum (Sum, extractOther, lower, lowerLeft, lowerRight, sum) as S
import Control.Monad.Transition.Trans (TransitionT(..), runTransitionT)
import UI (Component, UI, liftUI)

combine
  :: forall w1 w2 m f a
   . Functor m
  => Comonad w1
  => Comonad w2
  => Component m w1 f a
  -> Component m w2 f a
  -> Component m (S.Sum w1 w2) f a
combine c1 c2 = S.sum (c1 <#> liftUI S.lowerLeft) (c2 <#> liftUI S.lowerRight)

liftLeft :: forall w1 w2 m. TransitionT w1 m ~> TransitionT (S.Sum w1 w2) m
liftLeft t = TransitionT (runTransitionT t <<< S.lowerLeft)

liftRight :: forall w1 w2 m. TransitionT w2 m ~> TransitionT (S.Sum w1 w2) m
liftRight t = TransitionT (runTransitionT t <<< S.lowerRight)

liftUILeft :: forall w1 w2 m. Comonad w1 => Functor m => UI m w1 ~> UI m (S.Sum w1 w2)
liftUILeft = liftUI S.lowerLeft

liftUIRight :: forall w1 w2 m. Comonad w2 => Functor m => UI m w2 ~> UI m (S.Sum w1 w2)
liftUIRight = liftUI S.lowerRight

moveLeft :: forall w1 w2 m. Comonad w1 => TransitionT (S.Sum w1 w2) m Unit
moveLeft = TransitionT \s -> extract (S.lowerLeft s) unit

moveRight :: forall w1 w2 m. Comonad w2 => TransitionT (S.Sum w1 w2) m Unit
moveRight = TransitionT \s -> extract (S.lowerRight s) unit
