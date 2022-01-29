module Zap.Exception.InvalidOption where

import qualified Zap.Vendor.Exception as Exception

newtype InvalidOption
    = InvalidOption String
    deriving (Eq, Show)

instance Exception.Exception InvalidOption
