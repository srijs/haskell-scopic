{-# LANGUAGE OverloadedStrings #-}

module Scopic.RPC where

import Control.Arrow
import Control.Monad.Trans

import Data.Aeson
import Data.Text (Text)

import Network.JsonRpc.Server

import Scopic.Core.Filter

input = Required "input" :+: Required "state" :+: ()
output (a, s) = object [ "output" .= a, "state" .= s ]
wrap f a s = fmap output . lift $ f (a, s)

injectMethod :: (Monad m, FromJSON s, ToJSON s, FromJSON a, ToJSON b)
             => Text -> Middleware s t m a b x y -> Method m
injectMethod name f = toMethod name (wrap $ inject f) input

projectMethod :: (Monad m, FromJSON t, ToJSON t, FromJSON x, ToJSON y)
              => Text -> Middleware s t m a b x y -> Method m
projectMethod name f = toMethod name (wrap $ project f) input
