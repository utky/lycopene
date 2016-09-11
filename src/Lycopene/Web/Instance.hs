{-# LANGUAGE OverloadedStrings #-}
module Lycopene.Web.Instance where

import           Web.HttpApiData
import           Data.UUID
import           Data.Maybe (maybe)
import qualified Data.Text as T

instance FromHttpApiData UUID where
  parseUrlPiece = maybe (Left "UUID") Right . fromString . T.unpack
