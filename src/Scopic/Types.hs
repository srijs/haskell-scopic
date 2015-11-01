module Scopic.Types where

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
