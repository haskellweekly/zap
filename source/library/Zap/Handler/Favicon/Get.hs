module Zap.Handler.Favicon.Get where

import qualified Zap.Class.GetDataFileName as GetDataFileName
import qualified Zap.Vendor.Http as Http
import qualified Zap.Vendor.Text as Text
import qualified Zap.Vendor.Wai as Wai

handler :: GetDataFileName.GetDataFileName m => m Wai.Response
handler = do
    filePath <- GetDataFileName.getDataFileName "favicon.ico"
    pure $ Wai.responseFile
        Http.ok200
        [(Http.hContentType, Text.encodeUtf8 $ Text.pack "image/x-icon")]
        filePath
        Nothing
