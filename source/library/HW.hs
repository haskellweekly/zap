module HW where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
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
import qualified Text.XML as Xml

main :: IO ()
main = do
    config <- getConfig
    let settings = configToSettings config
    Warp.runSettings settings $ \request respond ->
        case Text.unpack <$> Wai.pathInfo request of
            [] -> case Http.parseMethod $ Wai.requestMethod request of
                Right Http.GET -> do
                    respond . Wai.responseLBS Http.ok200 [] $ Xml.renderLBS
                        Xml.def
                        Xml.Document
                            { Xml.documentPrologue = Xml.Prologue
                                { Xml.prologueBefore = []
                                , Xml.prologueDoctype = Nothing
                                , Xml.prologueAfter = []
                                }
                            , Xml.documentRoot = Xml.Element
                                { Xml.elementName = Xml.Name
                                    { Xml.nameLocalName = Text.pack "root"
                                    , Xml.nameNamespace = Nothing
                                    , Xml.namePrefix = Nothing
                                    }
                                , Xml.elementAttributes = Map.empty
                                , Xml.elementNodes = []
                                }
                            , Xml.documentEpilogue = []
                            }
                _ -> respond $ Wai.responseLBS
                    Http.methodNotAllowed405
                    []
                    LazyByteString.empty
            ["favicon.ico"] ->
                case Http.parseMethod $ Wai.requestMethod request of
                    Right Http.GET -> do
                        filePath <- Package.getDataFileName "favicon.ico"
                        respond $ Wai.responseFile
                            Http.ok200
                            [ ( Http.hContentType
                              , Text.encodeUtf8 $ Text.pack "image/x-icon"
                              )
                            ]
                            filePath
                            Nothing
                    _ -> respond $ Wai.responseLBS
                        Http.methodNotAllowed405
                        []
                        LazyByteString.empty
            ["robots.txt"] ->
                case Http.parseMethod $ Wai.requestMethod request of
                    Right Http.GET -> do
                        filePath <- Package.getDataFileName "robots.txt"
                        respond $ Wai.responseFile
                            Http.ok200
                            [ ( Http.hContentType
                              , Text.encodeUtf8 $ Text.pack "text/plain"
                              )
                            ]
                            filePath
                            Nothing
                    _ -> respond $ Wai.responseLBS
                        Http.methodNotAllowed405
                        []
                        LazyByteString.empty
            ["static", "style"] ->
                case Http.parseMethod $ Wai.requestMethod request of
                    Right Http.GET -> do
                        filePath <- Package.getDataFileName "index.css"
                        respond $ Wai.responseFile
                            Http.ok200
                            [ ( Http.hContentType
                              , Text.encodeUtf8 $ Text.pack "text/css"
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
    result <- getConfigWith name arguments
    case result of
        Left message -> do
            putStrLn message
            Exit.exitSuccess
        Right config -> pure config

getConfigWith
    :: Exception.MonadThrow m
    => String
    -> [String]
    -> m (Either String Config.Config)
getConfigWith name arguments = do
    flags <- Flag.fromArguments arguments
    config <- Config.fromFlags flags
    pure $ if Config.help config
        then Left . List.dropWhileEnd Char.isSpace $ Console.usageInfo
            name
            Flag.options
        else if Config.version config
            then Left $ Version.showVersion Package.version
            else Right config

configToSettings :: Config.Config -> Warp.Settings
configToSettings config =
    Warp.defaultSettings
        & Warp.setBeforeMainLoop (beforeMainLoop config)
        & Warp.setHost (Config.host config)
        & Warp.setLogger logger
        & Warp.setPort (Config.port config)
        & Warp.setServerName ByteString.empty

beforeMainLoop :: Config.Config -> IO ()
beforeMainLoop config = putStrLn $ unwords
    [ "listening on"
    , show $ Config.host config
    , "port"
    , show $ Config.port config
    ]

logger :: Wai.Request -> Http.Status -> Maybe Integer -> IO ()
logger request status _ = putStrLn $ unwords
    [ show $ Http.statusCode status
    , Text.unpack . Text.decodeUtf8With Text.lenientDecode $ Wai.requestMethod
        request
    , Text.unpack . Text.decodeUtf8With Text.lenientDecode $ Wai.rawPathInfo
        request
    ]
