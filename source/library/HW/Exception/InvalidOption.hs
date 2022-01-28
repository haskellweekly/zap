module HW.Exception.InvalidOption where

import qualified HW.Vendor.Exception as Exception

newtype InvalidOption
    = InvalidOption String
    deriving (Eq, Show)

instance Exception.Exception InvalidOption
