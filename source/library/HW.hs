module HW where

import qualified Data.Char as Char
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Proxy as Proxy
import qualified Data.Version as Version
import qualified HW.Exception.InvalidMethod as InvalidMethod
import qualified HW.Exception.UnknownRoute as UnknownRoute
import qualified HW.Handler.Common as Common
import qualified HW.Handler.Favicon.Get as Favicon.Get
import qualified HW.Handler.Index.Get as Index.Get
import qualified HW.Handler.Robots.Get as Robots.Get
import qualified HW.Handler.Style.Get as Style.Get
import qualified HW.Handler.Template.Get as Template.Get
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
        method <-
            either (Exception.throwM . InvalidMethod.InvalidMethod) pure
            . Http.parseMethod
            $ Wai.requestMethod request
        route <- Route.fromPathInfo $ Wai.pathInfo request
        case route of
            Route.Index -> case method of
                Http.GET -> do
                    response <- Index.Get.handler
                    respond response
                _ -> respond $ statusResponse Http.methodNotAllowed405 []
            Route.Favicon -> case method of
                Http.GET -> do
                    response <- Favicon.Get.handler
                    respond response
                _ -> respond $ statusResponse Http.methodNotAllowed405 []
            Route.Robots -> case method of
                Http.GET -> do
                    response <- Robots.Get.handler
                    respond response
                _ -> respond $ statusResponse Http.methodNotAllowed405 []
            Route.Style -> case method of
                Http.GET -> do
                    response <- Style.Get.handler
                    respond response
                _ -> respond $ statusResponse Http.methodNotAllowed405 []
            Route.Template -> case method of
                Http.GET -> do
                    response <- Template.Get.handler
                    respond response
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
    | isExceptionType
        (Proxy.Proxy :: Proxy.Proxy InvalidMethod.InvalidMethod)
        e
    = statusResponse Http.methodNotAllowed405 []
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

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers =
    Common.xmlResponse status headers . Common.elementToDocument $ Xml.Element
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
