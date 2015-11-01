{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Scopic.Core.Filter
  ( Filter, EndoFilter
  , null, filter
  , embed, project
  , mapL, mapR
  , swap, transform
  ) where

import Prelude hiding (id, (.), filter, null)

import Control.Arrow
import Control.Category
import Control.Monad.State.Strict

data Filter m s t a b = Filter
  { inj :: Kleisli (StateT s m) a b
  , prj :: Kleisli (StateT t m) b a
  }
type EndoFilter m s t a = Filter m s t a a

instance Monad m => Category (Filter m s t) where
  id = Filter id id
  Filter sg tg . Filter sf tf = Filter (sg . sf) (tf . tg)

instance Monad m => Monoid (EndoFilter m s t a) where
  mempty = id
  mappend = (.)

null :: Monad m => EndoFilter m s t ()
null = id

filter :: (a -> s -> m (b, s)) -> (b -> t -> m (a, t)) -> Filter m s t a b
filter f g = Filter (Kleisli $ StateT . f) (Kleisli $ StateT . g)

embed :: Filter m s t a b -> a -> s -> m (b, s)
embed (Filter inj _) = runStateT . runKleisli inj

project :: Filter m s t a b -> b -> t -> m (a, t)
project (Filter _ prj) = runStateT . runKleisli prj

swap :: Filter m s t a b -> Filter m t s b a
swap (Filter sf tf) = Filter tf sf

transform :: Monad m => (forall a. m a -> n a) -> Filter m s t a b -> Filter n s t a b
transform f (Filter sf tf) = Filter (xf sf) (xf tf)
  where xf k = Kleisli $ mapStateT f . runKleisli k

bimap :: Monad m => (a -> c) -> (c -> a) -> (b -> d) -> (d -> b) -> Filter m s t a b -> Filter m s t c d
bimap f f' g g' (Filter qf sf) = Filter (f' ^>> qf >>^ g) (g' ^>> sf >>^ f)

mapL :: Monad m => (a -> c) -> (c -> a) -> Filter m s t a b -> Filter m s t c b
mapL f g = bimap f g id id

mapR :: Monad m => (b -> d) -> (d -> b) -> Filter m s t a b -> Filter m s t a d
mapR = bimap id id
