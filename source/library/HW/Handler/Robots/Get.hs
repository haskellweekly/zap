module HW.Handler.Robots.Get where

import qualified HW.Class.GetDataFileName as GetDataFileName
import qualified HW.Vendor.Http as Http
import qualified HW.Vendor.Text as Text
import qualified HW.Vendor.Wai as Wai

handler :: GetDataFileName.GetDataFileName m => m Wai.Response
handler = do
    filePath <- GetDataFileName.getDataFileName "robots.txt"
    pure $ Wai.responseFile
        Http.ok200
        [ ( Http.hContentType
          , Text.encodeUtf8 $ Text.pack "text/plain; charset=UTF-8"
          )
        ]
        filePath
        Nothing
