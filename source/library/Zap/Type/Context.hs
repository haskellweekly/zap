module Zap.Type.Context where

import qualified Zap.Type.Config as Config

newtype Context = Context
    { config :: Config.Config
    } deriving (Eq, Show)

fromConfig :: Applicative m => Config.Config -> m Context
fromConfig cfg = pure Context { config = cfg }
