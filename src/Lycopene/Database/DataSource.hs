{-# LANGUAGE RankNTypes      #-}
module Lycopene.Database.DataSource
      ( DataSource
      , connect
      ) where

import           Database.HDBC (IConnection, ConnWrapper(..))
import           Database.HDBC.Sqlite3 (connectSqlite3)
-- import           Lycopene.Database.Datapath (tempDatapath)
-- import           Lycopene.Database.Schema (schema)

type DataSource = ConnWrapper

-- | Connect database with specified configuration
connect :: FilePath -> IO DataSource
connect fp = fmap dataSource $ connectSqlite3 fp
  where
    dataSource :: forall conn. (IConnection conn) => conn -> DataSource
    dataSource = ConnWrapper
