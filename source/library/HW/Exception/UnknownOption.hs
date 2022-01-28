module HW.Exception.UnknownOption where

import qualified HW.Vendor.Exception as Exception

newtype UnknownOption
    = UnknownOption String
    deriving (Eq, Show)

instance Exception.Exception UnknownOption
