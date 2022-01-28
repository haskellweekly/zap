module HW.Handler.Common where

import qualified HW.Vendor.Http as Http
import qualified HW.Vendor.Text as Text
import qualified HW.Vendor.Wai as Wai
import qualified HW.Vendor.Xml as Xml

elementToDocument :: Xml.Element -> Xml.Document
elementToDocument element = Xml.Document
    { Xml.documentPrologue = Xml.Prologue
        { Xml.prologueBefore =
            [ Xml.MiscInstruction Xml.Instruction
                  { Xml.instructionTarget = Text.pack "xml-stylesheet"
                  , Xml.instructionData = Text.pack
                      "type='text/xsl' href='/static/template'"
                  }
            ]
        , Xml.prologueDoctype = Nothing
        , Xml.prologueAfter = []
        }
    , Xml.documentRoot = element
    , Xml.documentEpilogue = []
    }

xmlResponse
    :: Http.Status -> Http.ResponseHeaders -> Xml.Document -> Wai.Response
xmlResponse status headers = Wai.responseLBS status headers
    . Xml.renderLBS Xml.def { Xml.rsPretty = True }
