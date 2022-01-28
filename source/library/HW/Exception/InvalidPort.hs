module HW.Exception.InvalidPort where

import qualified HW.Vendor.Exception as Exception

newtype InvalidPort
    = InvalidPort String
    deriving (Eq, Show)

instance Exception.Exception InvalidPort
