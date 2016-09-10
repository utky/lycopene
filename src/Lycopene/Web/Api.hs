{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Lycopene.Web.Api where

import           Data.Bifunctor (first)
import           Control.Monad.Except (ExceptT(ExceptT), withExceptT)
import           Servant
import           Lycopene.Application (AppEngine, runEngine)
import qualified Lycopene.Core as Core
import           Lycopene.Database (DBException(..))

type LycopeneApi
  =    "ping" :> Get '[JSON] String
  :<|> "projects" :> Get '[JSON] [Core.Project]

handleDBExc :: DBException -> ServantErr
handleDBExc (SqlE e)    = err500 { errBody = "Database error." }
handleDBExc (DecodeE e) = err500 { errBody = "Database fetch failure." }

api :: Proxy LycopeneApi
api = Proxy

server :: AppEngine -> Server LycopeneApi
server engine
  =    ping
  :<|> projects
  where
    withApp = withExceptT handleDBExc . ExceptT . runEngine engine
    ping = return "pong"
    projects = withApp $ Core.EProject Core.AllProject

