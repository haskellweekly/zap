module HW.Extra.Http where

import qualified Network.HTTP.Types as Http
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import qualified Data.CaseInsensitive as CI

hLink :: Http.HeaderName
hLink = CI.mk . Text.encodeUtf8 $ Text.pack "hLink"
