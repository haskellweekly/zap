module HW.Exception.UnexpectedArgument where

import qualified HW.Vendor.Exception as Exception

newtype UnexpectedArgument
    = UnexpectedArgument String
    deriving (Eq, Show)

instance Exception.Exception UnexpectedArgument
