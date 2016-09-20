{-# LANGUAGE RankNTypes #-}
-- | Driver to run domain with database interaction.
module Lycopene.Application
  ( AppEngine
  , runEngine
  , appEngine
  , defaultEngine
  ) where

import qualified Lycopene.Core as Core
import           Lycopene.Environment (dataPath)
import           Lycopene.Database (DataSource, connect, runDB, DBException(..))
import           Lycopene.Database.Relational (persist)

-- | Middleware which can process domain event.
newtype AppEngine =
  AppEngine { unEngine :: forall a. Core.Event a -> IO (Either DBException a) }

runEngine :: AppEngine -> Core.Event a -> IO (Either DBException a)
runEngine = unEngine

appEngine :: DataSource -> AppEngine
appEngine ds =
   let runDatabase = flip runDB
   in  AppEngine $ runDatabase ds . persist . Core.processEvent

defaultEngine :: IO AppEngine
defaultEngine = do
  dpath <- dataPath
  ds <- connect dpath
  return $ appEngine ds

