module HW.Type.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified HW.Exception.InvalidPort as InvalidPort
import qualified HW.Type.Flag as Flag
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Read as Read

data Config = Config
    { help :: Bool
    , port :: Warp.Port
    , version :: Bool
    }
    deriving (Eq, Show)

initial :: Config
initial = Config { help = False, port = 49152, version = False }

fromFlags :: (Foldable t, Exception.MonadThrow m) => t Flag.Flag -> m Config
fromFlags = Monad.foldM applyFlag initial

applyFlag :: Exception.MonadThrow m => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
    Flag.Help -> pure config { help = True }
    Flag.Port x -> case Read.readMaybe x of
        Nothing -> Exception.throwM $ InvalidPort.InvalidPort x
        Just y -> pure config { port = y }
    Flag.Version -> pure config { version = True }
