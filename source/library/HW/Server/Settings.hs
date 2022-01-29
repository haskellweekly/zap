module HW.Server.Settings where

import Data.Function ((&))
import qualified Data.Proxy as Proxy
import qualified HW.Exception.DisallowedMethod as DisallowedMethod
import qualified HW.Exception.InvalidMethod as InvalidMethod
import qualified HW.Exception.UnknownRoute as UnknownRoute
import qualified HW.Handler.Common as Common
import qualified HW.Type.Config as Config
import qualified HW.Vendor.ByteString as ByteString
import qualified HW.Vendor.Exception as Exception
import qualified HW.Vendor.Http as Http
import qualified HW.Vendor.Text as Text
import qualified HW.Vendor.Wai as Wai
import qualified HW.Vendor.Warp as Warp

fromConfig :: Config.Config -> Warp.Settings
fromConfig config =
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
    | isExceptionType
        (Proxy.Proxy :: Proxy.Proxy DisallowedMethod.DisallowedMethod)
        e
    = Common.statusResponse Http.methodNotAllowed405 []
    | isExceptionType
        (Proxy.Proxy :: Proxy.Proxy InvalidMethod.InvalidMethod)
        e
    = Common.statusResponse Http.methodNotAllowed405 []
    | isExceptionType (Proxy.Proxy :: Proxy.Proxy UnknownRoute.UnknownRoute) e
    = Common.statusResponse Http.notFound404 []
    | otherwise
    = Common.statusResponse Http.internalServerError500 []

isExceptionType
    :: Exception.Exception e
    => Proxy.Proxy e
    -> Exception.SomeException
    -> Bool
isExceptionType proxy someException =
    case Exception.fromException someException of
        Nothing -> False
        Just exception -> let _ = Proxy.asProxyTypeOf exception proxy in True
