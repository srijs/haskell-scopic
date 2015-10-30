{-# LANGUAGE RankNTypes #-}

module Scopic.Types where

import Control.Monad ((>=>))

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

type RequestFilter m = RequestInfo -> m RequestInfo
type ResponseFilter m = ResponseInfo -> m ResponseInfo

data Filter m = Filter (RequestFilter m) (ResponseFilter m)

instance Monad m => Monoid (Filter m) where
  mempty = Filter return return
  mappend (Filter reql resl) (Filter reqr resr) = Filter (reql >=> reqr) (resl >=> resr)

filterRequest :: Monad m => RequestFilter m -> Filter m
filterRequest f = Filter f return

filterResponse :: Monad m => ResponseFilter m -> Filter m
filterResponse = Filter return

transformFilter :: (forall a. m a -> n a) -> Filter m -> Filter n
transformFilter f (Filter reqf resf) = Filter (f . reqf) (f . resf)
