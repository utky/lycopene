{-# LANGUAGE RankNTypes #-}
module Lycopene.Core.Database.Query
                ( 
                ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader (MonadReader, ask)
import           Control.Monad.Trans

import           Database.Relational.Query
import           Database.HDBC.Record.Query (runQuery)
import           Database.HDBC.Record.Insert (runInsert)
import           Database.HDBC
import           Database.HDBC.Sqlite3

import           Lycopene.Core.Database.DataSource (connect, createTables)
import           Lycopene.Core.Database.Schema


{-
liftR r = ask >>= runQueryWithConn r

runQueryWithConn = lift . liftIO . withConnection

withConnection r conn = runQuery conn (relationalQuery r)
-}

-- a -> m [b]
-- m a -> m [b]

-------------------------------------------------------------------------------
-- it could be better because `unPersist fa conn` is repeated.
-- there is some misconception.

newtype Persist a = Persist { unPersist :: IConnection conn => conn -> IO a }

instance Functor Persist where
  f `fmap` fa = Persist runner where
    runner conn = fmap f (unPersist fa conn)

instance Applicative Persist where
  pure a = Persist $ \conn -> return a
  f <*> fa = Persist runner where
    runner conn = iof <*> iofa where
      iof  = unPersist f conn
      iofa = unPersist fa conn

instance Monad Persist where
  return = pure
  fa >>= f = Persist runner where
    runner conn = unPersist fa conn >>= f

-------------------------------------------------------------------------------

data SQL = Select (Relation

query :: (SqlValue r, Relation p r -> p -> Persist [r]
query r p conn = runnableQuery r p where
  runQuery'' q p conn = runQuery conn q p
  runnableQuery = (runQuery conn) . relationalQuery
