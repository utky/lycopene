{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Lycopene.Web.Api where

import           Servant
import           Lycopene.Application (AppEngine)
import           Lycopene.Web.Trans (lyco)
import           Lycopene.Web.Instance ()
import qualified Lycopene.Core as Core

type LycopeneApi
  =    "ping" :> Get '[JSON] String
  :<|> "projects" :> ProjectApi

type ProjectApi
  =    Get '[JSON] [Core.Project]
  :<|> Capture "name" Core.Name :> Get '[JSON] Core.Project
  :<|> ReqBody '[JSON] String :> Post '[JSON] Core.Project
  :<|> Capture "name" Core.Name :> DeleteNoContent '[JSON] NoContent

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
  where
    allProjects = lyco engine $ Core.EProject Core.AllProject
    fetchByName n = lyco engine $ Core.EProject (Core.FetchProject n)
    newProject n = lyco engine $ Core.EProject (Core.NewProject n Nothing)
    removeProject n = NoContent <$ (lyco engine $ Core.EProject (Core.RemoveProject n))
