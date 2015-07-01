{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Lycopene.Core.Database.Query
                ( queryP
                , relationP
                , insertP
                , updateP
                , module Lycopene.Core.Database.Persist
                ) where


import           Database.Relational.Query
import           Database.Record.FromSql
import           Database.Record.ToSql
import           Database.HDBC.Record.Query (runQuery)
import           Database.HDBC.Record.Insert (runInsert)
import           Database.HDBC.Record.KeyUpdate (runKeyUpdate)
import           Database.HDBC
import           Lycopene.Core.Database.Persist



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
