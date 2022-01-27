module HW.Type.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified HW.Type.Flag as Flag

data Config = Config
    { help :: Bool
    , version :: Bool
    }
    deriving (Eq, Show)

initial :: Config
initial = Config { help = False, version = False }

fromFlags :: (Foldable t, Exception.MonadThrow m) => t Flag.Flag -> m Config
fromFlags = Monad.foldM applyFlag initial

applyFlag :: Applicative m => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
    Flag.Help -> pure config { help = True }
    Flag.Version -> pure config { version = True }
