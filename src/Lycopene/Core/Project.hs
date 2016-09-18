{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
module Lycopene.Core.Project
  ( Project(..)
  , ProjectStatus(..)
  , ProjectId
  , ProjectF(..)
  , ProjectM
  , newProject
  , addProject
  , removeProject
  , updateProject
  , fetchByNameProject
  , fetchAllProject
  , activateProject
  , deactivateProject
  ) where

import           Data.Char (toLower)
import qualified Data.Text as T
import           GHC.Generics
import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.Aeson.Types
                   (typeMismatch, Value(..), Parser
                   , genericToEncoding, genericParseJSON, Options(..)
                   , defaultOptions, withText)
import           Lycopene.Core.Scalar
import           Lycopene.Freer (Freer, liftR, foldFreer)
import           Lycopene.Core.Store (Change)
import           Lycopene.Core.Identifier (generate, nameIdGen)
import           Lycopene.Core.Pure (VStore, VResult, runVStore, initial, values, save, fetch, remove, catch)
import           Lycopene.Lens (Lens, set, get, field)

type ProjectId = Identifier

data ProjectStatus 
  = ProjectInactive -- ^ Indicate that a project is not proceeding or completed.
  | ProjectActive -- ^ Indicate a project is working in progress.
  deriving (Eq, Ord, Show)

instance ToJSON ProjectStatus where
  toJSON ProjectActive = toJSON "active"
  toJSON ProjectInactive = toJSON "inactive"
  
instance FromJSON ProjectStatus where
  parseJSON = withText "active|inactive" $ \t ->
    case T.unpack t of
      "active"   -> return ProjectActive
      "inactive" -> return ProjectInactive
      _          -> fail "active|inactive"

data Project
  = Project
  { projectId   :: !ProjectId
  , projectName :: !Name
  , projectDescription :: Description
  , projectStatus :: !ProjectStatus
  }
  deriving (Show, Generic)

projectOptions :: Options
projectOptions =
  defaultOptions
    { fieldLabelModifier = map toLower . drop (length "project") }

instance ToJSON Project where
  toEncoding = genericToEncoding projectOptions
instance FromJSON Project where
  parseJSON = genericParseJSON projectOptions

-- Project is identified by `projectId`
instance Eq Project where
  x == y = (projectId x) == (projectId y)

-- Minimal lenses for Project
-- ==================================================================

_id :: Lens Project ProjectId
_id = field projectId (\a s -> s { projectId = a })

_name :: Lens Project Name
_name = field projectName (\a s -> s { projectName = a })

_status :: Lens Project ProjectStatus
_status = field projectStatus (\a s -> s { projectStatus = a })

-- | Operational primitives of Project
data ProjectF a where
  AddProjectF :: Project -> ProjectF Project
  RemoveProjectF :: Name -> ProjectF ()
  UpdateProjectF :: Change Project -> Project -> ProjectF Project
  FetchByNameProjectF :: Name -> ProjectF Project
  FetchAllProjectF :: ProjectF [Project]

type ProjectM = Freer ProjectF

-- Lifting boiler plate

newProject :: Name -> Description -> Project
newProject n d =
  let next = generate nameIdGen ("project", n)
  in  Project next n d ProjectActive

addProject :: Project -> ProjectM Project
addProject = liftR . AddProjectF

removeProject :: Name -> ProjectM ()
removeProject = liftR . RemoveProjectF

updateProject :: Change Project -> ProjectM Project -> ProjectM Project
updateProject f = (>>= (liftR . UpdateProjectF f))

fetchByNameProject :: Name -> ProjectM Project
fetchByNameProject = liftR . FetchByNameProjectF

fetchAllProject :: ProjectM [Project]
fetchAllProject = liftR FetchAllProjectF

activateProject :: ProjectM Project -> ProjectM Project
activateProject = (>>= (liftR . UpdateProjectF (set _status ProjectActive)))

deactivateProject :: ProjectM Project -> ProjectM Project
deactivateProject = (>>= (liftR . UpdateProjectF (set _status ProjectInactive)))
