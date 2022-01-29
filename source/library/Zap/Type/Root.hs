module Zap.Type.Root where

import qualified Zap.Type.Meta as Meta
import qualified Zap.Vendor.Map as Map
import qualified Zap.Vendor.Text as Text
import qualified Zap.Vendor.Xml as Xml

data Root = Root
    { meta :: Meta.Meta
    , page :: [Xml.Node]
    }
    deriving (Eq, Show)

toDocument :: Root -> Xml.Document
toDocument root = Xml.Document
    { Xml.documentPrologue = Xml.Prologue
        { Xml.prologueBefore =
            [ Xml.MiscInstruction Xml.Instruction
                  { Xml.instructionTarget = Text.pack "xml-stylesheet"
                  , Xml.instructionData = Text.pack
                      "type=\"text/xsl\" href=\"/static/template\""
                  }
            ]
        , Xml.prologueDoctype = Nothing
        , Xml.prologueAfter = []
        }
    , Xml.documentRoot = Xml.Element
        { Xml.elementName = Xml.Name
            { Xml.nameLocalName = Text.pack "root"
            , Xml.nameNamespace = Nothing
            , Xml.namePrefix = Nothing
            }
        , Xml.elementAttributes = Map.empty
        , Xml.elementNodes =
            [ Xml.NodeElement . Meta.toElement $ meta root
            , Xml.NodeElement Xml.Element
                { Xml.elementName = Xml.Name
                    { Xml.nameLocalName = Text.pack "page"
                    , Xml.nameNamespace = Nothing
                    , Xml.namePrefix = Nothing
                    }
                , Xml.elementAttributes = Map.empty
                , Xml.elementNodes = page root
                }
            ]
        }
    , Xml.documentEpilogue = []
    }
