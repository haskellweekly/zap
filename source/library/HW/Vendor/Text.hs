{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module HW.Vendor.Text
    ( module Data.Text
    , module Data.Text.Encoding
    , module Data.Text.Encoding.Error
    ) where

import Data.Text
import Data.Text.Encoding
import Data.Text.Encoding.Error hiding (replace)
