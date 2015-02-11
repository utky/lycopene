module Lycopene.Core.Project.Service where

import           Lycopene.Core.Monad
import           Lycopene.Core.Database
import           Lycopene.Core.Project.Entity


allProjects :: LycopeneT Persist [Project]
allProjects = liftL $ relationP project ()

addProject :: Project -> LycopeneT Persist Integer
addProject p = liftL $ insertP insertProject p
