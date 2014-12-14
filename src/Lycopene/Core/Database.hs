module Lycopene.Core.Database
        ( initDatabase
        ) where

import           Database.Persist.Sqlite

import           Lycopene.Core.Monad
import           Lycopene.Core.Entity
import           Lycopene.Core.Persist

initDatabase :: LycopeneT IO ()
initDatabase = runDB $ runMigration migrateAll

