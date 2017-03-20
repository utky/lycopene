{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Lycopene.Web.Api where

import           Servant
import           Lycopene.Application (AppEngine)
import           Lycopene.Web.Trans (lyco)
import           Lycopene.Web.Request
import qualified Lycopene.Core as Core


type LycopeneApi
  =    "ping" :> Get '[JSON] String
  :<|> "projects" :> ProjectApi
  :<|> "sprints" :> SprintApi
  :<|> "issues" :> IssueApi
  :<|> Raw

type ProjectApi
  =    Get '[JSON] [Core.Project]
  :<|> Capture "name" Core.Name :> Get '[JSON] Core.Project
  :<|> ReqBody '[JSON] String :> Post '[JSON] Core.Project
  :<|> Capture "name" Core.Name :> DeleteNoContent '[JSON] NoContent

type SprintApi
  =    QueryParam "project" Core.Name :> Get '[JSON] [Core.Sprint]
  :<|> QueryParam "project" Core.Name :> Capture "name" Core.Name :> Get '[JSON] Core.Sprint

type IssueApi
  =    QueryParam "project" Core.Name :> QueryParam "sprint" Core.Name :> QueryParam "status" Core.IssueStatus :> Get '[JSON] [Core.Issue]
  :<|> Capture "id" Core.IssueId :> Get '[JSON] Core.Issue
  -- :<|> ReqBody '[JSON] String :> Post '[JSON] Core.Issue
  :<|> Capture "id" Core.IssueId :> DeleteNoContent '[JSON] NoContent

api :: Proxy LycopeneApi
api = Proxy

server :: FilePath -> AppEngine -> Server LycopeneApi
server dir engine
  =    ping
  :<|> projectServer engine
  :<|> sprintServer engine
  :<|> issueServer engine
  :<|> serveDirectory dir
  where
    ping = return "pong"

projectServer :: AppEngine -> Server ProjectApi
projectServer engine
  =    allProjects
  :<|> fetchByName
  :<|> newProject
  :<|> removeProject
  where
    allProjects = lyco engine $ Core.AllProject
    fetchByName n = lyco engine $ (Core.FetchProject n)
    newProject n = lyco engine $ Core.NewProject n Nothing
    removeProject n = NoContent <$ (lyco engine $ Core.RemoveProject n)

sprintServer :: AppEngine -> Server SprintApi
sprintServer engine
  =    fetchSprints
  :<|> fetchSprint
  where
    fetchSprints (Just pj) = lyco engine $ (Core.FetchProjectSprint pj)
    fetchSprints Nothing = throwError $ err400 { errBody = "project name missing" }
    fetchSprint (Just pj) sp = lyco engine $ (Core.FetchSprint pj sp)
    fetchSprint Nothing _ = throwError $ err400 { errBody = "project name missing" }

issueServer :: AppEngine -> Server IssueApi
issueServer engine
  =    fetchIssues
  :<|> fetchIssue
  -- :<|> newIssue
  :<|> removeIssue
  where
    fetchIssues (Just pj) (Just sp) (Just st) = lyco engine $ (Core.FetchIssues pj sp st)
    fetchIssues (Just pj) (Just sp) Nothing  = lyco engine $ (Core.FetchIssues pj sp Core.IssueOpen)
    fetchIssues Nothing _ _ = throwError $ err400 { errBody = "project name missing" }
    fetchIssues _ Nothing _ = throwError $ err400 { errBody = "sprint name missing" }
    fetchIssue issueId = lyco engine $ Core.FetchIssue issueId
    -- newIssue t = lyco engine $ Core.NewIssue pj sp t
    removeIssue issueId = NoContent <$ (lyco engine $ Core.RemoveIssue issueId)
