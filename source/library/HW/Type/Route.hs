module HW.Type.Route where

import qualified HW.Exception.UnknownRoute as UnknownRoute
import qualified HW.Vendor.Exception as Exception
import qualified HW.Vendor.Text as Text

data Route
    = Favicon
    | Index
    | Robots
    | Style
    | Template
    deriving (Eq, Show)

fromPathInfo :: Exception.MonadThrow m => [Text.Text] -> m Route
fromPathInfo xs = case fmap Text.unpack xs of
    [] -> pure Index
    ["favicon.ico"] -> pure Favicon
    ["robots.txt"] -> pure Robots
    ["static", "style"] -> pure Style
    ["static", "template"] -> pure Template
    _ -> Exception.throwM $ UnknownRoute.UnknownRoute xs
