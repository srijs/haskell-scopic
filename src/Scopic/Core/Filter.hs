{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scopic.Core.Filter
  ( Handler, Filter, filter, pre, post
  , Middleware(..), inject, project
  , compose, connect
  , swap, transform
  ) where

import Prelude hiding (id, (.), filter, null, map)

import Control.Arrow
import Control.Category
import Control.Monad.State.Strict

import Data.Profunctor

type Handler s t m a b = (a, s) -> m (b, t)

newtype Filter r m a b = Filter { runFilter :: Kleisli (StateT r m) a b }
  deriving (Category, Arrow, Profunctor)

instance Monad m => Functor (Filter r m a) where
  fmap = rmap

filter :: Handler r r m a b -> Filter r m a b
filter f = Filter . Kleisli $ StateT . curry f

handle :: Filter r m a b -> Handler r r m a b
handle f = uncurry $ runStateT . runKleisli (runFilter f)

pre :: Monad m => Filter s m a b -> Handler s t m b c -> Handler s t m a c
pre f g = handle f >=> g

post :: Monad m => Handler s t m a b -> Filter t m b c -> Handler s t m a c
post f g = f >=> handle g

data Middleware s t m a b x y = Middleware
  { inj :: Filter s m a b
  , prj :: Filter t m x y
  }

inject :: Middleware s t m a b x y -> (a, s) -> m (b, s)
inject = handle . inj

project :: Middleware s t m a b x y -> (x, t) -> m (y, t)
project = handle . prj

compose :: Monad m
        => Middleware s t m b c x y
        -> Middleware s t m a b y z
        -> Middleware s t m a c x z
compose v w = Middleware (inj v <<< inj w) (prj v >>> prj w)

connect :: Monad m => Middleware s t m a b x y -> Handler s t m b x -> Handler s t m a y
connect w f = inj w `pre` f `post` prj w

swap :: Middleware s t m a b x y -> Middleware t s m x y a b
swap (Middleware sf tf) = Middleware tf sf

transform :: Monad m => (forall a. m a -> n a) -> Filter r m a b -> Filter r n a b
transform f (Filter k) = Filter . Kleisli $ mapStateT f . runKleisli k
