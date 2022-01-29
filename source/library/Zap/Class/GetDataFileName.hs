module Zap.Class.GetDataFileName where

import qualified Paths_zap as Package

class Monad m => GetDataFileName m where
    getDataFileName :: FilePath -> m FilePath

instance GetDataFileName IO where
    getDataFileName = Package.getDataFileName
