module Lycopene.Core.Persist where 

--import           Control.Monad.IO.Class  (liftIO)
--import           Database.Persist
import           Database.Persist.Sqlite
import           Data.Text (pack)
import           System.FilePath

import           Lycopene.Core.Entity
import           Lycopene.Configuration


defaultDatapath :: Configuration -> FilePath
defaultDatapath config = lycoHome config </> "issues.db"

initDatabase :: FilePath -> IO ()
initDatabase datapath = runSqlite (pack datapath) $ do
  runMigration migrateAll


-- type PersistM m a = SqlPersistT (NoLoggingT (ResourceT m)) a

{- | Run Persistent context with configuration.
-- 
runPersist :: Configuration -> SqlPersistT m a -> m a
runPersist c = runSqlite $ conn c where
  conn = (sqlDatabase . configureSqlite) c
  pool = createSqlitePool conn 3
-}

-- connect :: Configuration -> Text
-- connect = sqlDatabase . configureSqlite

-- configureSqlite :: Configuration -> SqliteConf
-- configureSqlite c = SqliteConf dataPath 3 where
--   dataPath = (lycoHome c) </> "issues.db"
