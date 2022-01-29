module Zap.Exception.InvalidMethod where

import qualified Zap.Vendor.ByteString as ByteString
import qualified Zap.Vendor.Exception as Exception

newtype InvalidMethod
    = InvalidMethod ByteString.ByteString
    deriving (Eq, Show)

instance Exception.Exception InvalidMethod
