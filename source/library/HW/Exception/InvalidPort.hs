module HW.Exception.InvalidPort where

import qualified Control.Monad.Catch as Exception

newtype InvalidPort
    = InvalidPort String
    deriving (Eq, Show)

instance Exception.Exception InvalidPort
