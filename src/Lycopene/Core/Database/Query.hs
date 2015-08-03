{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Lycopene.Core.Database.Query
                ( queryP
                , relationP
                , insertP
                , updateP
                , kupdateP
                , module Lycopene.Core.Database.Persist
                ) where


import           Database.Relational.Query
import           Database.Record.FromSql
import           Database.Record.ToSql
import           Database.HDBC.Record
-- import           Database.HDBC.Record.Query (runQuery)
-- import           Database.HDBC.Record.Insert (runInsert)
-- import           Database.HDBC.Record.KeyUpdate (runKeyUpdate)
import           Database.HDBC
import           Lycopene.Core.Database.Persist


instance ShowConstantTermsSQL Integer where
  showConstantTermsSQL' = let f :: Integer -> Int
                              f = fromInteger
                          in showConstantTermsSQL' . f

-- Lift method
-- ----------------------------------------------------------------

-- |
queryP :: (FromSql SqlValue a, ToSql SqlValue p) => Query p a -> p -> Persist [a]
queryP q p = Persist $ runnableQuery q p where
  runnableQuery qry parameter conn = runQuery conn qry parameter

-- |
relationP :: (FromSql SqlValue a, ToSql SqlValue p) => Relation p a -> p -> Persist [a]
relationP r = queryP (relationalQuery r)

-- |
insertP :: ToSql SqlValue a => Insert a -> a -> Persist Integer
insertP i a = Persist $ runnableInsert i a where
  runnableInsert ins entity conn = runInsert conn ins entity

-- |
kupdateP :: ToSql SqlValue a => KeyUpdate p a -> a -> Persist Integer
kupdateP k a = Persist $ runnableKeyUpdate k a where
  runnableKeyUpdate kupd entity conn = runKeyUpdate conn kupd entity

updateP :: ToSql SqlValue p => Update p -> p -> Persist Integer
updateP u p = Persist $ \c -> runUpdate c u p


-------------------------------------------------------------------------------
-- | Transient data
-- class Transient a where
--   initial :: 
--   tableDef :: Table a

-- rule1 :: (a -> b) -> Pi t b -> 

-- persistBase :: Insert 
