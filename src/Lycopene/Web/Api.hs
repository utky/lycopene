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
  :<|> Capture "id" Core.Id :> Get '[JSON] Core.Project
  :<|> ReqBody '[JSON] PostProject :> Post '[JSON] Core.Project
  :<|> Capture "id" Core.Id :> DeleteNoContent '[JSON] NoContent

type SprintApi
  =    QueryParam "project" Core.Id :> Get '[JSON] [Core.Sprint]
  :<|> Capture "id" Core.Id :> Get '[JSON] Core.Sprint

type IssueApi
  =    QueryParam "project" Core.Id :> QueryParam "sprint" Core.Id :> QueryParam "status" Core.IssueStatus :> Get '[JSON] [Core.Issue]
  :<|> Capture "id" Core.IssueId :> Get '[JSON] Core.Issue
  :<|> ReqBody '[JSON] PostIssue :> Post '[JSON] Core.Issue
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
  :<|> fetch
  :<|> newProject
  :<|> removeProject
  where
    allProjects = lyco engine $ Core.AllProject
    -- FIXME ここもIDでもらう方がいいかな
    fetch i = lyco engine $ (Core.FetchProject i)
    newProject (PostProject n d) = lyco engine $ Core.NewProject n d
    removeProject i = NoContent <$ (lyco engine $ Core.RemoveProject i)

sprintServer :: AppEngine -> Server SprintApi
sprintServer engine
  =    fetchSprints
  :<|> fetchSprint
  where
    fetchSprints (Just pj) = lyco engine $ (Core.FetchProjectSprint pj)
    -- FIXME: fetch all active sprint
    fetchSprints Nothing = throwError $ err400 { errBody = "project id missing" }
    fetchSprint sp = lyco engine $ (Core.FetchSprint sp)

issueServer :: AppEngine -> Server IssueApi
issueServer engine
  =    fetchIssues
  :<|> fetchIssue
  :<|> newIssue
  :<|> removeIssue
  where
    fetchIssues (Just pj) spm (Just st) = lyco engine $ (Core.FetchIssues pj spm st)
    fetchIssues (Just pj) spm Nothing  = lyco engine $ (Core.FetchIssues pj spm Core.IssueOpen)
    fetchIssues Nothing _ _ = throwError $ err400 { errBody = "project id missing" }
    fetchIssue issueId = lyco engine $ Core.FetchIssue issueId
    newIssue (PostIssue n d pj sp) = lyco engine $ Core.NewIssue n d pj sp
    removeIssue issueId = NoContent <$ (lyco engine $ Core.RemoveIssue issueId)
