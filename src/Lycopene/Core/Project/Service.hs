module Lycopene.Core.Project.Service where

import           Lycopene.Core.Monad
import           Lycopene.Core.Database
import qualified Lycopene.Core.Project.Entity as E
import qualified Lycopene.Core.Project.Value as V


allProjects :: LycopeneT Persist [E.Project]
allProjects = liftL $ relationP E.project ()

addProject :: E.Project -> LycopeneT Persist Integer
addProject p = liftL $ insertP E.insertProject p

inbox = liftL $ insertP E.insertProject E.Project
  { E.projectId = 1
  , E.name = "inbox"
  , E.description = Just "The global default project."
  }
