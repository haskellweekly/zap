module Zap.Server.Application where

import qualified Zap.Exception.DisallowedMethod as DisallowedMethod
import qualified Zap.Exception.InvalidMethod as InvalidMethod
import qualified Zap.Handler.Favicon.Get as Favicon.Get
import qualified Zap.Handler.Index.Get as Index.Get
import qualified Zap.Handler.Robots.Get as Robots.Get
import qualified Zap.Handler.Style.Get as Style.Get
import qualified Zap.Handler.Template.Get as Template.Get
import qualified Zap.Type.Context as Context
import qualified Zap.Type.Route as Route
import qualified Zap.Vendor.Exception as Exception
import qualified Zap.Vendor.Http as Http
import qualified Zap.Vendor.Wai as Wai

fromContext :: Context.Context -> Wai.Application
fromContext _ request respond = do
    method <-
        either (Exception.throwM . InvalidMethod.InvalidMethod) pure
        . Http.parseMethod
        $ Wai.requestMethod request
    route <- Route.fromPathInfo $ Wai.pathInfo request
    handler <- getHandler method route
    response <- handler
    respond response

getHandler
    :: Exception.MonadThrow m
    => Http.StdMethod
    -> Route.Route
    -> m (IO Wai.Response)
getHandler method route = case route of
    Route.Favicon -> case method of
        Http.GET -> pure Favicon.Get.handler
        _ -> Exception.throwM $ DisallowedMethod.DisallowedMethod method
    Route.Index -> case method of
        Http.GET -> pure Index.Get.handler
        _ -> Exception.throwM $ DisallowedMethod.DisallowedMethod method
    Route.Robots -> case method of
        Http.GET -> pure Robots.Get.handler
        _ -> Exception.throwM $ DisallowedMethod.DisallowedMethod method
    Route.Style -> case method of
        Http.GET -> pure Style.Get.handler
        _ -> Exception.throwM $ DisallowedMethod.DisallowedMethod method
    Route.Template -> case method of
        Http.GET -> pure Template.Get.handler
        _ -> Exception.throwM $ DisallowedMethod.DisallowedMethod method
