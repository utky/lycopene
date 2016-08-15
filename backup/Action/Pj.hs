module Lycopene.Action.Pj where

import           Lycopene.Action
import qualified Lycopene.Core.Project as Project


listProjects :: Action [Project.Project]
listProjects = domain Project.allProjects
