module HW.Exception.UnknownRoute where

import qualified HW.Vendor.Exception as Exception
import qualified HW.Vendor.Text as Text

newtype UnknownRoute
    = UnknownRoute [Text.Text]
    deriving (Eq, Show)

instance Exception.Exception UnknownRoute
