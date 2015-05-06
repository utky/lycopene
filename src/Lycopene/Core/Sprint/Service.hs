module Lycopene.Core.Sprint.Service where

import           Lycopene.Core.Monad
import           Lycopene.Core.Database
import qualified Lycopene.Core.Sprint.Entity as E
import qualified Lycopene.Core.Project as Project

-- | Aggregation of use-case "create project and backlog sprint"
-- It returns created project ID
newProjectAndSprint :: String -> (Maybe String) -> LycopeneT Persist (Integer, Integer)
newProjectAndSprint n d = do
  let sprintname = "backlog"
  _ <- Project.newProject (Project.ProjectV n d)
  projectId <- (Project.projectId . head) `fmap` Project.projectByName n
  _ <- newSprint (E.SprintV { E.vName = sprintname
                            , E.vDescription = Just $ n ++ "-" ++ sprintname
                            , E.vProjectId = projectId
                            , E.vStartOn = Nothing
                            , E.vEndOn = Nothing
                            })
  sprintId <- (E.sprintId . head) `fmap` sprintByProjectAndName projectId sprintname
  _ <- liftL $ insertP E.insertBacklogSprint (E.BacklogSprint projectId sprintId)
  return (projectId, sprintId)
  

newSprint :: E.SprintV -> LycopeneT Persist Integer
newSprint sv = liftL $ insertP E.insertSprintV sv

sprintByProjectAndName :: Integer -> String -> LycopeneT Persist [E.Sprint]
sprintByProjectAndName pj nm = liftL $ relationP E.selectByProjectAndName (pj, nm)

inboxBacklog :: LycopeneT Persist Integer
inboxBacklog = liftL $ insertP E.insertBacklogSprint (E.BacklogSprint 0 0)

inboxDefault :: LycopeneT Persist Integer
inboxDefault = liftL $ insertP E.insertSprint E.Sprint
                                              { E.sprintId = 0
                                              , E.name = "backlog"
                                              , E.description = Just "inbox-backlog"
                                              , E.projectId = 0
                                              , E.startOn = Nothing
                                              , E.endOn = Nothing
                                              }

-- | projectId -> sprintId (as backlog)
getBacklogSprint :: Integer -> LycopeneT Persist (Maybe Integer)
getBacklogSprint projectId = liftL $ fmap headId (relationP E.selectBacklogByProject projectId) where
  headId (id':_) = Just id'
  headId []     = Nothing
