{-# LANGUAGE MultiParamTypeClasses #-}
-- | Lift operations that lift up values from database to domain.
module Lycopene.Database.Relational.Lift where

import           Data.Maybe (fromMaybe)
import           Data.UUID (fromString)
import           Lycopene.Database (Persist)
import qualified Lycopene.Database.Relational.Project as Project
import qualified Lycopene.Core as Core

class LiftCore a b where
  liftC :: a -> Maybe b

-- instance (LiftCore a b) => LiftCore [a] [b] where
--   liftC = fmap liftC


instance LiftCore Project.Project Core.Project where
  liftC p = Just $ Core.Project 
              { Core.projectId = Project.projectId p
              , Core.projectName = Project.name p
              , Core.projectDescription = Project.description p
              , Core.projectStatus = liftC $ Project.status p
              }

instance LiftCore Integer Core.ProjectStatus where
  liftC 0 = Just Core.Inactive
  liftC 1 = Just Core.Active
