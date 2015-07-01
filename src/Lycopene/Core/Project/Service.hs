module Lycopene.Core.Project.Service where

import           Lycopene.Core.Monad
import           Lycopene.Core.Database
import qualified Lycopene.Core.Project.Entity as E


allProjects :: Lycopene [E.Project]
allProjects = runPersist $ relationP E.project ()

newProject :: E.ProjectV -> Lycopene Integer
newProject p = runPersist $ insertP E.insertProjectV p

projectByName :: String -> Lycopene [E.Project]
projectByName name = runPersist $ relationP E.selectByName name

inbox :: Lycopene Integer
inbox = runPersist $ insertP E.insertProject E.Project
                                        { E.projectId = 0
                                        , E.name = "inbox"
                                        , E.description = Just "The global default project."
                                        }
