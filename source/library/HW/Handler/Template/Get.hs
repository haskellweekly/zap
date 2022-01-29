module HW.Handler.Template.Get where

import qualified HW.Vendor.Http as Http
import qualified HW.Vendor.Text as Text
import qualified HW.Vendor.Wai as Wai
import qualified Paths_hw as Package

handler :: IO Wai.Response
handler = do
    filePath <- Package.getDataFileName "index.xsl"
    pure $ Wai.responseFile
        Http.ok200
        [ ( Http.hContentType
          , Text.encodeUtf8 $ Text.pack "text/xsl; charset=UTF-8"
          )
        ]
        filePath
        Nothing
