module HW.Exception.DisallowedMethod where

import qualified HW.Vendor.Exception as Exception
import qualified HW.Vendor.Http as Http

newtype DisallowedMethod
    = DisallowedMethod Http.StdMethod
    deriving (Eq, Show)

instance Exception.Exception DisallowedMethod
