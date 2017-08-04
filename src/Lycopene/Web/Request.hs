{-# LANGUAGE DeriveGeneric #-}
module Lycopene.Web.Request where

import           Data.Char (toLower)
import           GHC.Generics
import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.Aeson.Types
                   ( genericToEncoding
                   , genericParseJSON
                   , Options(..)
                   , defaultOptions
                   , withText)

import qualified Lycopene.Core as Core

prefixFieldOptions :: String -> Options
prefixFieldOptions p = 
  let modifyField = lowerFirst . drop (length p)
      lowerFirst (x:xs) = (toLower x) : xs
  in  defaultOptions { fieldLabelModifier = modifyField }

data PostProject =
    PostProject
    { postProjectName :: Core.Name
    , postProjectDescription :: Core.Description
    } deriving (Show, Generic)

projectOptions :: Options
projectOptions = prefixFieldOptions "postProject"

instance FromJSON PostProject where
  parseJSON = genericParseJSON projectOptions

data PostIssue =
    PostIssue
    { postIssueTitle       :: Core.Name
    , postIssueDescription :: Core.Description
    , postProjectProjectId :: Core.ProjectId
    , postProjectSprintId  :: Maybe Core.SprintId
    } deriving (Show, Generic)

issueOptions :: Options
issueOptions = prefixFieldOptions "postIssue"

instance FromJSON PostIssue where
  parseJSON = genericParseJSON issueOptions
