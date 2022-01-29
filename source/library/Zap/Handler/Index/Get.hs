module Zap.Handler.Index.Get where

import qualified Zap.Handler.Common as Common
import qualified Zap.Package as Package
import qualified Zap.Type.Meta as Meta
import qualified Zap.Type.Root as Root
import qualified Zap.Vendor.Http as Http
import qualified Zap.Vendor.Text as Text
import qualified Zap.Vendor.Wai as Wai

handler :: Applicative m => m Wai.Response
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
        $ Root.toDocument Root.Root
              { Root.meta = Meta.Meta { Meta.version = Package.version }
              , Root.page = []
              }
