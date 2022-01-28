module HW.Exception.InvalidMethod where

import qualified HW.Vendor.ByteString as ByteString
import qualified HW.Vendor.Exception as Exception

newtype InvalidMethod
    = InvalidMethod ByteString.ByteString
    deriving (Eq, Show)

instance Exception.Exception InvalidMethod
