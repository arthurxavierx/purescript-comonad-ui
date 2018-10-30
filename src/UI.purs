module UI where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Trans.Class (class ComonadTrans, lower)
import Control.Extend (extend)
import Control.Monad.Transition.Trans (TransitionT, hoistTransitionT, pairTransitionT)

-- | A `Handler` runs transitions on a monad `m`.
-- |
-- | It is essencially a natural transformation from `TransitionT w m` to `m`,
-- | but, for practical reasons, here it is defined as a transformation of
-- | purely effectful transitions into purely effectful actions on `m`.
type Handler m w = TransitionT w m Unit -> m Unit

-- | A `UI` is a function which outputs a description of type `a` of an
-- | interface given a way to handle the transitions dispatched by this
-- | interface through means of a `Handler`.
type UI m w a = Handler m w -> a

-- | A component is a comonad `w` representing the space of all possible future
-- | `UI`s. It is a space (or context) containing `UI`s, and the shape of this
-- | space (given by the shape of `w`) gives the possibilities for `UI`s in this
-- | component.
-- |
-- | For example, the component given by the `Stream` comonad below is one
-- | where its `UI`s only follow a predefined sequence:
-- |
-- | ```purescript
-- | data Stream a = Cons a (Lazy (Stream a))
-- | ```
-- |
-- | In this way, a `Component` is a comonad `w` containing a current `UI` and
-- | future `UI`s whose `Handler`s handle (in a `m` monad) transitions
-- | dispoatched by an interface of type `a`.
type Component m w f a = w (UI m w (f a))

-- | TODO: write documentation
explore
  :: forall m w f a
   . Comonad w
  => Monad m
  => m (Component m w f a)
  -> (Component m w f a -> m Unit)
  -> Component m w f a
  -> f a
explore read write space = extract space send
  where
    send :: Handler m w
    send transition =
      read
      >>= pairTransitionT (const identity) transition <<< extend pure
      >>= write

-- | TODO: write documentation
explore_
  :: forall m w f a
   . Comonad w
  => Monad m
  => (Component m w f a -> m Unit)
  -> Component m w f a
  -> f a
explore_ write space = explore (pure space) write space

-- | TODO: write documentation
liftUI
  :: forall m w wp
   . Comonad w
  => Functor m
  => wp ~> w
  -> UI m w ~> UI m wp
liftUI lower ui = \send -> ui (send <<< hoistTransitionT lower)

-- | TODO: write documentation
liftUIT
  :: forall m w wp
   . Comonad w
  => Functor m
  => ComonadTrans wp
  => UI m w ~> UI m (wp w)
liftUIT = liftUI lower

-- | TODO: write documentation
liftComponent
  :: forall m w wp f a
   . Comonad w
  => Functor m
  => wp ~> w
  -> Component m w f a -> UI m wp (f a)
liftComponent lower = liftUI lower <<< extract

-- | TODO: write documentation
liftComponentT
  :: forall m w wp f a
   . Comonad w
  => Functor m
  => ComonadTrans wp
  => Component m w f a -> UI m (wp w) (f a)
liftComponentT = liftComponent lower
