module HW.Handler.Index.Get where

import qualified HW.Handler.Common as Common
import qualified HW.Type.Root as Root
import qualified HW.Vendor.Http as Http
import qualified HW.Vendor.Text as Text
import qualified HW.Vendor.Wai as Wai

handler :: IO Wai.Response
handler = do
    pure
        . Common.xmlResponse
              Http.ok200
              [ ( Http.hContentType
                , Text.encodeUtf8 $ Text.pack "text/xml; charset=UTF-8"
                )
              , ( Http.hLink
                , Text.encodeUtf8
                    $ Text.pack "</static/style>; rel=preload; as=style"
                )
              ]
        $ Root.toDocument Root.Root { Root.nodes = [] }
