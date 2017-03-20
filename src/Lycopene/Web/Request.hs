{-# LANGUAGE OverloadedStrings #-}
module Lycopene.Web.Request where

import           Web.HttpApiData
import           Data.UUID
import           Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Lycopene.Core as Core

instance FromHttpApiData UUID where
  parseUrlPiece = maybe (Left "UUID") Right . fromString . T.unpack

instance FromHttpApiData Core.IssueStatus where
  parseQueryParam = parse where
    parse t =
      case T.unpack t of
        "open"   -> Right Core.IssueOpen
        "closed" -> Right Core.IssueClosed
        _        -> Left "IssueStatus"

data NewIssueRequest =
    NewIssueRequest
    { project :: Core.Name
    , sprint :: Core.Name
    , title :: String
    }
