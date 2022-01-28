module HW.Handler.Index.Get where

import qualified HW.Handler.Common as Common
import qualified HW.Vendor.Http as Http
import qualified HW.Vendor.Map as Map
import qualified HW.Vendor.Text as Text
import qualified HW.Vendor.Wai as Wai
import qualified HW.Vendor.Xml as Xml

handler :: IO Wai.Response
handler = do
    pure . Common.xmlResponse Http.ok200 [] $ Common.elementToDocument
        Xml.Element
            { Xml.elementName = Xml.Name
                { Xml.nameLocalName = Text.pack "root"
                , Xml.nameNamespace = Nothing
                , Xml.namePrefix = Nothing
                }
            , Xml.elementAttributes = Map.empty
            , Xml.elementNodes = []
            }
