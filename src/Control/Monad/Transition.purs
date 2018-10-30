-- | Monads from comonads,
-- | based on <https://github.com/paf31/purescript-pairing>.

module Control.Monad.Transition
  ( Transition
  , runTransition
  , liftTransition
  , lowerTransition
  , hoistTransition
  , pairTransition
  , module T
  ) where

import Prelude

import Control.Comonad (class Comonad)
import Control.Monad.Transition.Trans (TransitionT(..), hoistTransitionT, liftTransitionT, lowerTransitionT, pairTransitionT, runTransitionT) as T
import Data.Identity (Identity(..))
import Data.Newtype (un)

-- | TODO: write documentation
type Transition w a = T.TransitionT w Identity a

-- | TODO: write documentation
transition :: forall w a. (forall r. w (a -> r) -> r) -> Transition w a
transition = T.TransitionT

-- | TODO: write documentation
runTransition :: forall w a r. Functor w => Transition w a -> w (a -> r) -> r
runTransition t = un Identity <<< T.runTransitionT t <<< map (Identity <<< _)

-- | TODO: write documentation
liftTransition :: forall w s. Comonad w => (forall a. w a -> s) -> Transition w s
liftTransition = T.liftTransitionT

-- | TODO: write documentation
lowerTransition :: forall w a s. Functor w => Transition w s -> w a -> s
lowerTransition t = un Identity <<< T.lowerTransitionT t

-- | TODO: write documentation
hoistTransition :: forall w v. v ~> w -> Transition w ~> Transition v
hoistTransition = T.hoistTransitionT

-- | TODO: write documentation
pairTransition :: forall w a b c. Functor w => (a -> b -> c) -> Transition w a -> w b -> c
pairTransition f t = un Identity <<< T.pairTransitionT (\a b -> Identity (f a b)) t
