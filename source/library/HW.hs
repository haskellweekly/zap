module HW where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_hw as Package

main :: IO ()
main = Warp.runSettings Warp.defaultSettings $ \ request respond ->
    case Text.unpack <$> Wai.pathInfo request of
        ["favicon.ico"] ->
            case Http.parseMethod $ Wai.requestMethod request of
                Right Http.GET -> do
                    filePath <- Package.getDataFileName "favicon.ico"
                    respond $ Wai.responseFile Http.ok200 [(Http.hContentType, Encoding.encodeUtf8 $ Text.pack "image/x-icon")] filePath Nothing
                _ -> respond $ Wai.responseLBS Http.methodNotAllowed405 [] LazyByteString.empty
        _ -> respond $ Wai.responseLBS Http.notFound404 [] LazyByteString.empty
