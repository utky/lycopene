{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Lycopene.Web.Trans where

import           Servant
import           Servant.Utils.Enter ((:~>)(Nat), enter)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.Except (ExceptT(ExceptT), withExceptT)
import           Lycopene.Application (AppEngine, runEngine)
import qualified Lycopene.Core as Core
import           Lycopene.Database (DBException(..))

type WithEngine = ReaderT AppEngine

type LycoHandler a = Core.Event a -> Handler a

lyco :: AppEngine -> LycoHandler a
lyco engine = enter (withApp engine)

withApp :: AppEngine -> Core.Event :~> Handler
withApp engine = Nat $ withExceptT handleDBExc . ExceptT . runEngine engine

handleDBExc :: DBException -> ServantErr
handleDBExc (SqlE e)    = err500 { errBody = "Database error." }
handleDBExc (DecodeE e) = err500 { errBody = "Database fetch failure." }
