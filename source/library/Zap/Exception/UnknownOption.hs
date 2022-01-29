module Zap.Exception.UnknownOption where

import qualified Zap.Vendor.Exception as Exception

newtype UnknownOption
    = UnknownOption String
    deriving (Eq, Show)

instance Exception.Exception UnknownOption
