{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Lycopene.Web.Api where

import           Control.Monad.Except (ExceptT(ExceptT), withExceptT)
import           Servant
import           Lycopene.Application (AppEngine)
import           Lycopene.Web.Trans (lyco)
import           Lycopene.Web.Instance
import qualified Lycopene.Core as Core
import           Lycopene.Database (DBException(..))

type LycopeneApi
  =    "ping" :> Get '[JSON] String
  :<|> "projects" :> ProjectApi

type ProjectApi
  =    Get '[JSON] [Core.Project]
  :<|> ReqBody '[JSON] String :> Post '[JSON] Core.Project
  :<|> Capture "id" Core.Identifier :> DeleteNoContent '[JSON] NoContent

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
  =    (lyco engine $ Core.EProject Core.AllProject)
  :<|> (\n -> lyco engine $ Core.EProject (Core.NewProject n Nothing))
  :<|> (\i -> NoContent <$ (lyco engine $ Core.EProject (Core.RemoveProject i)))
