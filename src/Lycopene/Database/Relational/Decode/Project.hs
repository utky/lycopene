module Lycopene.Database.Relational.Decode.Project (project) where

import           Data.UUID (fromString)
import           Lycopene.Database.Relational.Decode.Prim
import qualified Lycopene.Core as Core
import qualified Lycopene.Database.Relational.Project as Pj

project :: Decode Pj.Project Core.Project
project =
  Core.Project
    <$> decoder (fromString . Pj.projectId) <?> "projectId"
    <*> decoder Pj.name
    <*> decoder Pj.description
    <*> decoder (projectStatus . Pj.status) <?> "projectStatus" 

projectStatus :: Integer -> Maybe Core.ProjectStatus
projectStatus 0 = Just Core.ProjectInactive
projectStatus 1 = Just Core.ProjectActive
projectStatus _ = Nothing
