{-# LANGUAGE RankNTypes      #-}
module Lycopene.Database.DataSource
      ( DataSource
      , connect
      ) where

import           Database.HDBC (IConnection, ConnWrapper(..), runRaw)
import           Database.HDBC.Sqlite3 (connectSqlite3)

type DataSource = ConnWrapper

sqlitePragma :: String
sqlitePragma = "COMMIT; PRAGMA foreign_keys = ON; BEGIN TRANSACTION"

-- | Connect database with specified configuration
--
connect :: FilePath -> IO DataSource
connect fp = fmap dataSource $ preparePragma =<< connectSqlite3 fp
  where
    preparePragma conn = do
      -- hack to enable foreign key constraint
      runRaw conn sqlitePragma >>
        return conn
    dataSource :: forall conn. (IConnection conn) => conn -> DataSource
    dataSource = ConnWrapper
