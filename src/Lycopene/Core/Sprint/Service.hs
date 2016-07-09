module Lycopene.Core.Sprint.Service where

import           Lycopene.Core.Monad
import           Lycopene.Core.Context
import           Lycopene.Core.Database
import qualified Lycopene.Core.Sprint as E
import qualified Lycopene.Core.Project as Project

defaultSprintName :: String
defaultSprintName = "backlog"

-- | Aggregation of use-case "create project and backlog sprint"
-- It returns created project ID
newProjectAndSprint :: String -> (Maybe String) -> Lycopene (Integer, Integer)
newProjectAndSprint n d = do
  _ <- Project.newProject (Project.ProjectV n d)
  projectId <- (Project.projectId . head) `fmap` Project.projectByName n
  _ <- newSprint (E.SprintV { E.vName = defaultSprintName
                            , E.vDescription = Just $ n ++ "-" ++ defaultSprintName
                            , E.vProjectId = projectId
                            , E.vStartOn = Nothing
                            , E.vEndOn = Nothing
                            })
  sprintId <- (E.sprintId . head) `fmap` sprintByProjectAndName projectId defaultSprintName
  _ <- runPersist $ insertP E.insertBacklogSprint (E.BacklogSprint projectId sprintId)
  return (projectId, sprintId)
  

newSprint :: E.SprintV -> Lycopene Integer
newSprint sv = runPersist $ insertP E.insertSprintV sv

sprintByProjectAndName :: Integer -> String -> Lycopene [E.Sprint]
sprintByProjectAndName pj nm = runPersist $ relationP E.selectByProjectAndName (pj, nm)

sprintByProject :: Lycopene [E.Sprint]
sprintByProject = do
  pj <- fmap targetProject context
  runPersist $ relationP E.selectByProject pj

inboxBacklog :: Lycopene Integer
inboxBacklog = runPersist $ insertP E.insertBacklogSprint (E.BacklogSprint 0 0)

inboxDefault :: Lycopene Integer
inboxDefault = runPersist $ insertP E.insertSprint E.Sprint
                                              { E.sprintId = 0
                                              , E.name = "backlog"
                                              , E.description = Just "inbox-backlog"
                                              , E.projectId = 0
                                              , E.startOn = Nothing
                                              , E.endOn = Nothing
                                              }

-- | projectId -> sprintId (as backlog)
getBacklogSprint :: Integer -> Lycopene (Maybe Integer)
getBacklogSprint projectId = runPersist $ fmap headId (relationP E.selectBacklogByProject projectId) where
  headId (id':_) = Just id'
  headId []     = Nothing
