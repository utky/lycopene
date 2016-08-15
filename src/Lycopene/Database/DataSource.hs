{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Lycopene.Database.DataSource
      ( DataSource
      , mkDataSource
      , withDataSource
      , withDataSourceTx
      , connect
      , createTables
      ) where

import           Database.HDBC (IConnection, ConnWrapper(..), runRaw, withTransaction)
import           Lycopene.Database.Datapath (tempDatapath)
import           Lycopene.Database.Schema (schema)
import           Lycopene.Configuration


type DataSource = ConnWrapper

mkDataSource :: forall conn. (IConnection conn) => conn -> DataSource
mkDataSource = ConnWrapper

withDataSource :: DataSource -> (forall conn. (IConnection conn) => conn -> a) -> a
withDataSource = flip ($)

withDataSourceTx :: DataSource -> (forall conn. (IConnection conn) => conn -> IO a) -> IO a
withDataSourceTx = withTransaction


-- | Connect database with specified configuration
connect :: Configuration -> IO Connection
connect = connectSqlite3 . datapath

createTables :: IConnection conn => conn -> IO ()
createTables = flip runRaw schema
