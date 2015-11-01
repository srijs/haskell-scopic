{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scopic.Core.Filter
  ( Handler, Filter, filter, handle
  , Middleware(..)
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

filter :: Handler r r m a b -> Filter r m a b
filter f = Filter . Kleisli $ StateT . curry f

handle :: Filter r m a b -> Handler r r m a b
handle f = uncurry $ runStateT . runKleisli (runFilter f)

data Middleware s t m a b x y = Middleware
  { inj :: Filter s m a b
  , prj :: Filter t m x y
  }

injK = Kleisli . handle . inj
prjK = Kleisli . handle . prj

compose :: Monad m
        => Middleware s t m b c x y
        -> Middleware s t m a b y z
        -> Middleware s t m a c x z
compose v w = Middleware (inj v <<< inj w) (prj v >>> prj w)

connect :: Monad m => Middleware s t m a b x y -> Handler s t m b x -> Handler s t m a y
connect w f = runKleisli $ injK w >>> Kleisli f >>> prjK w

swap :: Middleware s t m a b x y -> Middleware t s m x y a b
swap (Middleware sf tf) = Middleware tf sf

transform :: Monad m => (forall a. m a -> n a) -> Filter r m a b -> Filter r n a b
transform f (Filter k) = Filter . Kleisli $ mapStateT f . runKleisli k
