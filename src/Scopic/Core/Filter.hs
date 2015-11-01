{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Scopic.Core.Filter
  ( Filter, EndoFilter
  , runL, runR
  , bimap, swap, transform
  ) where

import Prelude hiding (id, (.))

import Control.Arrow
import Control.Category
import Control.Monad.State.Strict

type F r m a b = Kleisli (StateT r m) a b
data Filter m s t a b = Filter (F s m a b) (F t m b a)
type EndoFilter m s t a = Filter m s t a a

swap :: Filter m s t a b -> Filter m t s b a
swap (Filter sf tf) = Filter tf sf

instance Monad m => Category (Filter m s t) where
  id = Filter id id
  Filter sg tg . Filter sf tf = Filter (sg . sf) (tf . tg)

instance Monad m => Monoid (EndoFilter m s t a) where
  mempty = id
  mappend = (.)

transform :: Monad m => (forall a. m a -> n a) -> Filter m s t a b -> Filter n s t a b
transform f (Filter sf tf) = Filter (xf sf) (xf tf)
  where xf k = Kleisli $ mapStateT f . runKleisli k

runL :: Filter m s t a b -> a -> s -> m (b, s)
runL (Filter sf _) = runStateT . runKleisli sf

runR :: Filter m s t a b -> b -> t -> m (a, t)
runR (Filter _ tf) = runStateT . runKleisli tf

bimap :: Monad m => (a -> c) -> (c -> a) -> (b -> d) -> (d -> b) -> Filter m s t a b -> Filter m s t c d
bimap f f' g g' (Filter qf sf) = Filter (f' ^>> qf >>^ g) (g' ^>> sf >>^ f)
