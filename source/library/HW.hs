module HW where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.runSettings Warp.defaultSettings $ \ _ respond ->
    respond $ Wai.responseLBS Http.notImplemented501 [] LazyByteString.empty
