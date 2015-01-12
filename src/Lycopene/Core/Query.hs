module Lycopene.Core.Query
                ( runCreateTables
                ) where

import           Control.Applicative
import           Control.Monad.Reader (MonadReader, ask)
import           Control.Monad.Trans
import           Database.Relational.Query
import           Database.HDBC.Record.Query (runQuery)
import           Database.HDBC
import           Database.HDBC.Sqlite3

import           Lycopene.Core.Monad
import           Lycopene.Core.DataSource (connect, createTables)
import           Lycopene.Core.Schema
import           Lycopene.Configuration


{-
liftR r = ask >>= runQueryWithConn r

runQueryWithConn = lift . liftIO . withConnection

withConnection r conn = runQuery conn (relationalQuery r)
-}

-- a -> m [b]
-- m a -> m [b]

runCreateTables :: Configuration -> IO ()
runCreateTables cfg = connect cfg >>= ((flip withTransaction) createTables)

