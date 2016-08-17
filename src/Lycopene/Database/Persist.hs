{-# LANGUAGE RankNTypes       #-}
module Lycopene.Database.Persist where

import           Data.Typeable
import           Control.Exception (Exception)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.Trans (lift)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Database.HDBC (IConnection, runRaw, withTransaction, SqlError, catchSql)
import           Lycopene.Database.Relational.Decode (DecodeError(..))

-------------------------------------------------------------------------------
-- it could be better because `unPersist fa conn` is repeated.
-- there is some misconception.

type DB = ExceptT DBException Persist

db :: Persist a -> DB a
db = lift

data DBException
  = SqlE SqlError
  | DecodeE DecodeError
  deriving (Typeable)

instance Exception DBException

instance Show DBException where
  show (SqlE e) = "SqlException: " ++ (show e)
  show (DecodeE e) = "DecodeException: " ++ (show e)

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

runDB :: (IConnection conn , MonadIO m)
      => DB r -> conn -> m (Either DBException r)
runDB db conn = liftIO $
  (runPersist (runExceptT db) conn)
    `catchSql` (return . Left . SqlE)

-- | Run database middleware and gain result with side-effect.
-- This operation may fail.
runPersist :: (IConnection conn) => Persist r -> conn -> IO r
runPersist (Persist p) conn = withTransaction conn p

-- | Construct persistent operation with raw sql statements.
rawPersist :: String -> Persist ()
rawPersist sql = Persist (\conn -> runRaw conn sql)

rawDB :: String -> DB ()
rawDB = lift . rawPersist

