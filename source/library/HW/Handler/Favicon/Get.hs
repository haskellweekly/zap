module HW.Handler.Favicon.Get where

import qualified HW.Vendor.Http as Http
import qualified HW.Vendor.Text as Text
import qualified HW.Vendor.Wai as Wai
import qualified Paths_hw as Package

handler :: IO Wai.Response
handler = do
    filePath <- Package.getDataFileName "favicon.ico"
    pure $ Wai.responseFile
        Http.ok200
        [(Http.hContentType, Text.encodeUtf8 $ Text.pack "image/x-icon")]
        filePath
        Nothing
