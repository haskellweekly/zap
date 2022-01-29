module Zap.Exception.DisallowedMethod where

import qualified Zap.Vendor.Exception as Exception
import qualified Zap.Vendor.Http as Http

newtype DisallowedMethod
    = DisallowedMethod Http.StdMethod
    deriving (Eq, Show)

instance Exception.Exception DisallowedMethod
