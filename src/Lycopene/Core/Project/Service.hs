module Lycopene.Core.Project.Service where

import           Lycopene.Core.Monad
import           Lycopene.Core.Database
import qualified Lycopene.Core.Project.Entity as E


allProjects :: LycopeneT Persist [E.Project]
allProjects = liftL $ relationP E.project ()

newProject :: E.ProjectV -> LycopeneT Persist Integer
newProject p = liftL $ insertP E.insertProjectV p

projectByName :: String -> LycopeneT Persist [E.Project]
projectByName name = liftL $ relationP E.selectByName name

inbox :: LycopeneT Persist Integer
inbox = liftL $ insertP E.insertProject E.Project
                                        { E.projectId = 0
                                        , E.name = "inbox"
                                        , E.description = Just "The global default project."
                                        }
