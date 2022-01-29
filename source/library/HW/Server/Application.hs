module HW.Server.Application where

import qualified HW.Exception.DisallowedMethod as DisallowedMethod
import qualified HW.Exception.InvalidMethod as InvalidMethod
import qualified HW.Handler.Favicon.Get as Favicon.Get
import qualified HW.Handler.Index.Get as Index.Get
import qualified HW.Handler.Robots.Get as Robots.Get
import qualified HW.Handler.Style.Get as Style.Get
import qualified HW.Handler.Template.Get as Template.Get
import qualified HW.Type.Context as Context
import qualified HW.Type.Route as Route
import qualified HW.Vendor.Exception as Exception
import qualified HW.Vendor.Http as Http
import qualified HW.Vendor.Wai as Wai

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
