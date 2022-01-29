module Zap.Type.Meta where

import qualified Data.Version as Version
import qualified Zap.Vendor.Map as Map
import qualified Zap.Vendor.Text as Text
import qualified Zap.Vendor.Xml as Xml

newtype Meta = Meta
    { version :: Version.Version
    } deriving (Eq, Show)

toElement :: Meta -> Xml.Element
toElement meta = Xml.Element
    { Xml.elementName = Xml.Name
        { Xml.nameLocalName = Text.pack "meta"
        , Xml.nameNamespace = Nothing
        , Xml.namePrefix = Nothing
        }
    , Xml.elementAttributes = Map.empty
    , Xml.elementNodes =
        [ Xml.NodeElement Xml.Element
              { Xml.elementName = Xml.Name
                  { Xml.nameLocalName = Text.pack "version"
                  , Xml.nameNamespace = Nothing
                  , Xml.namePrefix = Nothing
                  }
              , Xml.elementAttributes = Map.empty
              , Xml.elementNodes =
                  [ Xml.NodeContent . Text.pack . Version.showVersion $ version
                        meta
                  ]
              }
        ]
    }
