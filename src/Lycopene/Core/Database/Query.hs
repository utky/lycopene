{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Lycopene.Core.Database.Query
                ( Persist
                , runPersist
                , direct
                , queryP
                , relationP
                , insertP
                , updateP
                ) where

import           Control.Applicative
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Database.Relational.Query
import           Database.Record.FromSql
import           Database.Record.ToSql
import           Database.HDBC.Record.Query (runQuery)
import           Database.HDBC.Record.Insert (runInsert)
import           Database.HDBC.Record.KeyUpdate (runKeyUpdate)
import           Database.HDBC

-------------------------------------------------------------------------------
-- it could be better because `unPersist fa conn` is repeated.
-- there is some misconception.

newtype Persist a = Persist { unPersist :: IConnection conn => conn -> IO a }

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

instance MonadIO Persist where
  liftIO ia = Persist $ \_ -> ia

runPersist :: IConnection conn => Persist a -> conn -> IO a
runPersist = unPersist
-------------------------------------------------------------------------------

direct :: (forall conn. IConnection conn => conn -> IO a) -> Persist a
direct = Persist

-- Lift method
-- ----------------------------------------------------------------

-- |
queryP :: (FromSql SqlValue a, ToSql SqlValue p) => Query p a -> p -> Persist [a]
queryP q p = Persist $ runnableQuery q p where
  runnableQuery qry parameter conn = runQuery conn qry parameter

-- |
relationP :: (FromSql SqlValue a, ToSql SqlValue p) => Relation p a -> p -> Persist [a]
relationP r p = queryP (relationalQuery r) p

-- |
insertP :: ToSql SqlValue a => Insert a -> a -> Persist Integer
insertP i a = Persist $ runnableInsert i a where
  runnableInsert ins entity conn = runInsert conn ins entity

-- |
updateP :: ToSql SqlValue a => KeyUpdate p a -> a -> Persist Integer
updateP k a = Persist $ runnableKeyUpdate k a where
  runnableKeyUpdate kupd entity conn = runKeyUpdate conn kupd entity

-------------------------------------------------------------------------------
-- | Transient data
-- class Transient a where
--   initial :: 
--   tableDef :: Table a

-- rule1 :: (a -> b) -> Pi t b -> 

-- persistBase :: Insert 
