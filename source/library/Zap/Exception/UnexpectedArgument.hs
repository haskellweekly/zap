module Zap.Exception.UnexpectedArgument where

import qualified Zap.Vendor.Exception as Exception

newtype UnexpectedArgument
    = UnexpectedArgument String
    deriving (Eq, Show)

instance Exception.Exception UnexpectedArgument
