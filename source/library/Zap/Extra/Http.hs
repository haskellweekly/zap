module Zap.Extra.Http where

import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as Http

hLink :: Http.HeaderName
hLink = CI.mk . Text.encodeUtf8 $ Text.pack "hLink"
