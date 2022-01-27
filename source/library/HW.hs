module HW where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Version as Version
import qualified HW.Type.Config as Config
import qualified HW.Type.Flag as Flag
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_hw as Package
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit

main :: IO ()
main = do
    config <- getConfig
    let settings = configToSettings config
    Warp.runSettings settings $ \request respond ->
        case Text.unpack <$> Wai.pathInfo request of
            ["favicon.ico"] ->
                case Http.parseMethod $ Wai.requestMethod request of
                    Right Http.GET -> do
                        filePath <- Package.getDataFileName "favicon.ico"
                        respond $ Wai.responseFile
                            Http.ok200
                            [ ( Http.hContentType
                              , Encoding.encodeUtf8 $ Text.pack "image/x-icon"
                              )
                            ]
                            filePath
                            Nothing
                    _ -> respond $ Wai.responseLBS
                        Http.methodNotAllowed405
                        []
                        LazyByteString.empty
            _ -> respond
                $ Wai.responseLBS Http.notFound404 [] LazyByteString.empty

getConfig :: IO Config.Config
getConfig = do
    name <- Environment.getProgName
    arguments <- Environment.getArgs
    flags <- Flag.fromArguments arguments
    config <- Config.fromFlags flags
    Monad.when (Config.help config) $ do
        putStr $ Console.usageInfo name Flag.options
        Exit.exitSuccess
    Monad.when (Config.version config) $ do
        putStrLn $ Version.showVersion Package.version
        Exit.exitSuccess
    pure config

configToSettings :: Config.Config -> Warp.Settings
configToSettings config =
    Warp.defaultSettings
        & Warp.setHost (Config.host config)
        & Warp.setPort (Config.port config)
        & Warp.setServerName ByteString.empty
