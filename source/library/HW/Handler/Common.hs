module HW.Handler.Common where

import qualified HW.Vendor.Http as Http
import qualified HW.Vendor.Wai as Wai
import qualified HW.Vendor.Xml as Xml

xmlResponse
    :: Http.Status -> Http.ResponseHeaders -> Xml.Document -> Wai.Response
xmlResponse status headers = Wai.responseLBS status headers
    . Xml.renderLBS Xml.def { Xml.rsPretty = True }
