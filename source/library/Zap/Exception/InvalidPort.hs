module Zap.Exception.InvalidPort where

import qualified Zap.Vendor.Exception as Exception

newtype InvalidPort
    = InvalidPort String
    deriving (Eq, Show)

instance Exception.Exception InvalidPort
