{-# LANGUAGE RankNTypes #-}

module Scopic.Types where

import Prelude hiding (id, (.))

import Control.Arrow
import Control.Category
import Control.Monad.State.Strict

import Data.ByteString

import Network.HTTP.Types

data RequestInfo = RequestInfo
  { reqMethod :: Method
  , reqURI :: ByteString
  , reqVersion :: HttpVersion
  , reqHeaders :: RequestHeaders
  }

data ResponseInfo = ResponseInfo
  { resVersion :: HttpVersion
  , resStatus :: Status
  , resHeaders :: ResponseHeaders
  }

type F r m a b = Kleisli (StateT r m) a b
data Filter m s t a b = Filter (F s m a b) (F t m b a)

swap :: Filter m s t a b -> Filter m t s b a
swap (Filter sf tf) = Filter tf sf

instance Monad m => Category (Filter m s t) where
  id = Filter id id
  Filter sg tg . Filter sf tf = Filter (sg . sf) (tf . tg)

transformFilter :: Monad m => (forall a. m a -> n a) -> Filter m s t a b -> Filter n s t a b
transformFilter f (Filter sf tf) = Filter (transform f sf) (transform f tf)
  where transform f k = Kleisli $ mapStateT f . runKleisli k

filterRequest :: Filter m s t a b -> a -> s -> m (b, s)
filterRequest (Filter sf _) = runStateT . runKleisli sf

filterResponse :: Filter m s t a b -> b -> t -> m (a, t)
filterResponse (Filter _ tf) = runStateT . runKleisli tf

bimap :: Monad m => (a -> c) -> (c -> a) -> (b -> d) -> (d -> b) -> Filter m s t a b -> Filter m s t c d
bimap f f' g g' (Filter qf sf) = Filter (f' ^>> qf >>^ g) (g' ^>> sf >>^ f)
