module HW where

import qualified Data.Char as Char
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Proxy as Proxy
import qualified Data.Version as Version
import qualified HW.Exception.UnknownRoute as UnknownRoute
import qualified HW.Type.Config as Config
import qualified HW.Type.Flag as Flag
import qualified HW.Type.Route as Route
import qualified HW.Vendor.ByteString as ByteString
import qualified HW.Vendor.Exception as Exception
import qualified HW.Vendor.Http as Http
import qualified HW.Vendor.Map as Map
import qualified HW.Vendor.Text as Text
import qualified HW.Vendor.Wai as Wai
import qualified HW.Vendor.Warp as Warp
import qualified HW.Vendor.Xml as Xml
import qualified Paths_hw as Package
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit

main :: IO ()
main = do
    config <- getConfig
    let settings = configToSettings config
    Warp.runSettings settings $ \request respond -> do
        route <- Route.fromPathInfo $ Wai.pathInfo request
        case route of
            Route.Index ->
                case Http.parseMethod $ Wai.requestMethod request of
                    Right Http.GET -> do
                        respond . xmlResponse Http.ok200 [] $ elementToDocument
                            Xml.Element
                                { Xml.elementName = Xml.Name
                                    { Xml.nameLocalName = Text.pack "root"
                                    , Xml.nameNamespace = Nothing
                                    , Xml.namePrefix = Nothing
                                    }
                                , Xml.elementAttributes = Map.empty
                                , Xml.elementNodes = []
                                }
                    _ -> respond $ statusResponse Http.methodNotAllowed405 []
            Route.Favicon ->
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
                    _ -> respond $ statusResponse Http.methodNotAllowed405 []
            Route.Robots ->
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
                    _ -> respond $ statusResponse Http.methodNotAllowed405 []
            Route.Style ->
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
                    _ -> respond $ statusResponse Http.methodNotAllowed405 []
            Route.Template ->
                case Http.parseMethod $ Wai.requestMethod request of
                    Right Http.GET -> do
                        filePath <- Package.getDataFileName "index.xsl"
                        respond $ Wai.responseFile
                            Http.ok200
                            [ ( Http.hContentType
                              , Text.encodeUtf8 $ Text.pack "text/xsl"
                              )
                            ]
                            filePath
                            Nothing
                    _ -> respond $ statusResponse Http.methodNotAllowed405 []

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
        & Warp.setOnExceptionResponse onExceptionResponse
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

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse e
    | isExceptionType (Proxy.Proxy :: Proxy.Proxy UnknownRoute.UnknownRoute) e
    = statusResponse Http.notFound404 []
    | otherwise
    = statusResponse Http.internalServerError500 []

isExceptionType
    :: Exception.Exception e
    => Proxy.Proxy e
    -> Exception.SomeException
    -> Bool
isExceptionType proxy someException =
    case Exception.fromException someException of
        Nothing -> False
        Just exception -> let _ = Proxy.asProxyTypeOf exception proxy in True

elementToDocument :: Xml.Element -> Xml.Document
elementToDocument element = Xml.Document
    { Xml.documentPrologue = Xml.Prologue
        { Xml.prologueBefore =
            [ Xml.MiscInstruction Xml.Instruction
                  { Xml.instructionTarget = Text.pack "xml-stylesheet"
                  , Xml.instructionData = Text.pack
                      "type='text/xsl' href='/static/template'"
                  }
            ]
        , Xml.prologueDoctype = Nothing
        , Xml.prologueAfter = []
        }
    , Xml.documentRoot = element
    , Xml.documentEpilogue = []
    }

xmlResponse
    :: Http.Status -> Http.ResponseHeaders -> Xml.Document -> Wai.Response
xmlResponse status headers = Wai.responseLBS status headers
    . Xml.renderLBS Xml.def { Xml.rsPretty = True }

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers =
    xmlResponse status headers . elementToDocument $ Xml.Element
        { Xml.elementName = Xml.Name
            { Xml.nameLocalName = Text.pack "status"
            , Xml.nameNamespace = Nothing
            , Xml.namePrefix = Nothing
            }
        , Xml.elementAttributes = Map.empty
        , Xml.elementNodes =
            [ Xml.NodeElement Xml.Element
                { Xml.elementName = Xml.Name
                    { Xml.nameLocalName = Text.pack "code"
                    , Xml.nameNamespace = Nothing
                    , Xml.namePrefix = Nothing
                    }
                , Xml.elementAttributes = Map.empty
                , Xml.elementNodes =
                    [ Xml.NodeContent . Text.pack . show $ Http.statusCode
                          status
                    ]
                }
            , Xml.NodeElement Xml.Element
                { Xml.elementName = Xml.Name
                    { Xml.nameLocalName = Text.pack "message"
                    , Xml.nameNamespace = Nothing
                    , Xml.namePrefix = Nothing
                    }
                , Xml.elementAttributes = Map.empty
                , Xml.elementNodes =
                    [ Xml.NodeContent
                      . Text.decodeUtf8With Text.lenientDecode
                      $ Http.statusMessage status
                    ]
                }
            ]
        }
