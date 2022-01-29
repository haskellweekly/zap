module Zap.Exception.UnknownRoute where

import qualified Zap.Vendor.Exception as Exception
import qualified Zap.Vendor.Text as Text

newtype UnknownRoute
    = UnknownRoute [Text.Text]
    deriving (Eq, Show)

instance Exception.Exception UnknownRoute
