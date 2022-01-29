module Zap.Handler.Robots.Get where

import qualified Zap.Class.GetDataFileName as GetDataFileName
import qualified Zap.Vendor.Http as Http
import qualified Zap.Vendor.Text as Text
import qualified Zap.Vendor.Wai as Wai

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
