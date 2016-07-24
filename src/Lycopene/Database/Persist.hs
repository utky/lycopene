{-# LANGUAGE RankNTypes       #-}
module Lycopene.Database.Persist where


import           Control.Monad.IO.Class (liftIO)
import           Database.HDBC (IConnection, runRaw, SqlValue, withTransaction, getTables)
import           Database.Relational.Query
import           Database.Record (ToSql, FromSql)
import           Database.HDBC.Record

import           Lycopene.Core.Monad
import           Lycopene.Core.Context
import           Lycopene.Database.DataSource (withDataSource)

-------------------------------------------------------------------------------
-- it could be better because `unPersist fa conn` is repeated.
-- there is some misconception.


newtype Persist a = Persist { unPersist :: forall conn. IConnection conn => conn -> IO a }

instance Functor Persist where
  f `fmap` fa = Persist runner where
    runner conn = fmap f (unPersist fa conn)

instance Applicative Persist where
  pure a = Persist $ \_ -> return a
  f <*> fa = Persist runner where
    runner conn = iof <*> iofa where
      iof  = unPersist f conn
      iofa = unPersist fa conn

instance Monad Persist where
  return = pure
  fa >>= f = Persist runner where
    ffb = f `fmap` fa
    runner conn = unwrap ffb >>= unwrap where
      unwrap = flip unPersist conn

-- | Run database middleware and gain result with side-effect.
runPersist :: ( IConnection conn , MonadIO m) => Persist r -> conn -> m r
runPersist p conn = liftIO (withTransaction conn (unPersist p))


-- Lift method
-- ----------------------------------------------------------------


-- | Lift 'relational-record' 'Insert' DSL into Persist monad
-- Integer means count of record inserted
insertPersist :: (ToSql SqlValue p) => Insert p -> p -> Persist Integer
insertPersist i p = Persist (\conn -> runInsert conn i p)

-- | Lift 'relational-record' 'InsertQuery' DSL into Persist monad
-- Integer means count of record inserted
insertQueryPersist :: (ToSql SqlValue p) => InsertQuery p -> p -> Persist Integer
insertQueryPersist i p = Persist (\conn -> runInsertQuery conn i p)

-- | Lift 'relational-record' 'Update' DSL into Persist monad
-- Integer means count of record updated
updatePersist :: (ToSql SqlValue p) => Update p -> p -> Persist Integer
updatePersist u p = Persist (\conn -> runUpdate conn u p)

-- | Lift 'relational-record' 'KeyUpdate' DSL into Persist monad
-- Integer means count of record updated
kupdatePersist :: (ToSql SqlValue a) => KeyUpdate p a -> a -> Persist Integer
kupdatePersist u k = Persist (\conn -> runKeyUpdate conn u k)

-- | Lift 'relational-record' 'Delete' DSL into Persist monad
-- Integer means count of record deleted
deletePersist :: (ToSql SqlValue p) => Delete p -> p -> Persist Integer
deletePersist d p = Persist (\conn -> runDelete conn d p)

-- | Lift 'relational-record' 'Relation' DSL into Persist monad.
-- Apply query parameter @p@ to 'Relation'
selectPersist :: (ToSql SqlValue p, FromSql SqlValue a) => Relation p a -> p -> Persist [a]
selectPersist q p = Persist (\conn -> runQuery conn (relationalQuery q) p)

rawPersist :: String -> Persist ()
rawPersist sql = Persist (\conn -> runRaw conn sql)

