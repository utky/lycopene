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

type ProjectApi
  =    Get '[JSON] [Core.Project]
  :<|> Capture "name" Core.Name :> Get '[JSON] Core.Project
  :<|> ReqBody '[JSON] String :> Post '[JSON] Core.Project
  :<|> Capture "name" Core.Name :> DeleteNoContent '[JSON] NoContent
  :<|> Capture "name" Core.Name :> "sprints" :> SprintApi

type SprintApi
  =    Get '[JSON] [Core.Sprint]
  :<|> Capture "name" Core.Name :> Get '[JSON] Core.Sprint
  :<|> Capture "name" Core.Name :> "issues" :> IssueApi

type IssueApi
  =    QueryParam "status" Core.IssueStatus :> Get '[JSON] [Core.Issue]
  :<|> ReqBody '[JSON] String :> Post '[JSON] Core.Issue

api :: Proxy LycopeneApi
api = Proxy

server :: AppEngine -> Server LycopeneApi
server engine
  =    ping
  :<|> projectServer engine
  where
    ping = return "pong"

projectServer :: AppEngine -> Server ProjectApi
projectServer engine
  =    allProjects
  :<|> fetchByName
  :<|> newProject
  :<|> removeProject
  :<|> sprintHandler
  where
    allProjects = lyco engine $ Core.AllProject
    fetchByName n = lyco engine $ (Core.FetchProject n)
    newProject n = lyco engine $ Core.NewProject n Nothing
    removeProject n = NoContent <$ (lyco engine $ Core.RemoveProject n)
    sprintHandler pj = sprintServer pj engine

sprintServer :: Core.Name -> AppEngine -> Server SprintApi
sprintServer pj engine
  =    fetchSprints
  :<|> fetchSprint
  :<|> issueHandler
  where
    fetchSprints = lyco engine $ (Core.FetchProjectSprint pj)
    fetchSprint sp = lyco engine $ (Core.FetchSprint pj sp)
    issueHandler sp = issueServer pj sp engine

issueServer :: Core.Name -> Core.Name -> AppEngine -> Server IssueApi
issueServer pj sp engine
  =    fetchIssues
  :<|> newIssue
  where
    fetchIssues (Just st) = lyco engine $ (Core.FetchIssues pj sp st)
    fetchIssues Nothing   = lyco engine $ (Core.FetchIssues pj sp Core.IssueOpen)
    newIssue t = lyco engine $ Core.NewIssue pj sp t
