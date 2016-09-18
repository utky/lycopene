{-# LANGUAGE FlexibleInstances #-}
module Lycopene.Database.Relational.Decode.Issue where

import           Data.UUID (fromString)
import           Lycopene.Database.Relational.Decode.Prim
import qualified Lycopene.Core as Core
import qualified Lycopene.Database.Relational.Issue as Is

issue :: Decode Is.Issue Core.Issue
issue =
  Core.Issue
    <$> decoder (fromString . Is.issueId) <?> "issueId"
    <*> decoder Is.title
    <*> decoder Is.description
    <*> decoder (issueStatus . Is.status) <?> "issueStatus" 

issueStatus :: Int -> Maybe Core.IssueStatus
issueStatus 0 = Just Core.IssueClosed
issueStatus 1 = Just Core.IssueOpen
issueStatus _ = Nothing


-- $(defineProductConstructorInstance
--  [t|Core.Project|]
--  [|Core.Project|]
--  -- TODO: enumerate TypeQ from reifying data constructor.
--  [ [t|Core.ProjectId|]
--  , [t|Core.Name|]
--  , [t|Core.Description|]
--  , [t|Core.ProjectStatus|]
--  ])


