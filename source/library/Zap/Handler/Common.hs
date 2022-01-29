module Zap.Handler.Common where

import qualified Zap.Type.Root as Root
import qualified Zap.Vendor.Http as Http
import qualified Zap.Vendor.Map as Map
import qualified Zap.Vendor.Text as Text
import qualified Zap.Vendor.Wai as Wai
import qualified Zap.Vendor.Xml as Xml

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers =
    xmlResponse
            status
            (( Http.hContentType
             , Text.encodeUtf8 $ Text.pack "text/xml; charset=UTF-8"
             )
            : headers
            )
        $ Root.toDocument Root.Root
              { Root.nodes =
                  [ Xml.NodeElement Xml.Element
                      { Xml.elementName = Xml.Name
                          { Xml.nameLocalName = Text.pack "code"
                          , Xml.nameNamespace = Nothing
                          , Xml.namePrefix = Nothing
                          }
                      , Xml.elementAttributes = Map.empty
                      , Xml.elementNodes =
                          [ Xml.NodeContent
                            . Text.pack
                            . show
                            $ Http.statusCode status
                          ]
                      }
                  , Xml.NodeElement Xml.Element
                      { Xml.elementName = Xml.Name
                          { Xml.nameLocalName = Text.pack "message"
                          , Xml.nameNamespace = Nothing
                          , Xml.namePrefix = Nothing
                          }
                      , Xml.elementAttributes = Map.empty
                      , Xml.elementNodes =
                          [ Xml.NodeContent
                            . Text.decodeUtf8With Text.lenientDecode
                            $ Http.statusMessage status
                          ]
                      }
                  ]
              }

xmlResponse
    :: Http.Status -> Http.ResponseHeaders -> Xml.Document -> Wai.Response
xmlResponse status headers = Wai.responseLBS status headers
    . Xml.renderLBS Xml.def { Xml.rsPretty = True }
