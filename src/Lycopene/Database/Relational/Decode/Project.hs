{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TemplateHaskell #-}
module Lycopene.Database.Relational.Decode.Project where

import           Data.UUID (fromString)
-- import           Database.Relational.Query.TH (defineProductConstructorInstance)
import           Lycopene.Database.Relational.Decode.Prim
import qualified Lycopene.Core as Core
import qualified Lycopene.Database.Relational.Project as Pj

project :: Decode Pj.Project Core.Project
project =
  Core.Project
    <$> decoder (fmap Core.uuid . fromString . Pj.projectId) <?> "projectId"
    <*> decoder Pj.name
    <*> decoder Pj.description
    <*> decoder (projectStatus . Pj.status) <?> "projectStatus" 

projectStatus :: Int -> Maybe Core.ProjectStatus
projectStatus 0 = Just Core.ProjectInactive
projectStatus 1 = Just Core.ProjectActive
projectStatus _ = Nothing


-- $(defineProductConstructorInstance
--  [t|Core.Project|]
--  [|Core.Project|]
--  -- TODO: enumerate TypeQ from reifying data constructor.
--  [ [t|Core.ProjectId|]
--  , [t|Core.Name|]
--  , [t|Core.Description|]
--  , [t|Core.ProjectStatus|]
--  ])


