{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Lycopene.Database.Query
                ( queryP
                , relationP
                , insertP
                , updateP
                , kupdateP
                , module Lycopene.Database.Persist
                ) where


import           Database.Relational.Query
import           Database.Record.FromSql
import           Database.Record.ToSql
import           Database.HDBC.Record
-- import           Database.HDBC.Record.Query (runQuery)
-- import           Database.HDBC.Record.Insert (runInsert)
-- import           Database.HDBC.Record.KeyUpdate (runKeyUpdate)

import           Database.HDBC
import           Lycopene.Core (Lycopene(..), ProjectF(..))
import           Lycopene.Database.Persist
import           Lycopene.Database.Project
import           Database.Relational.Query
import           Database.HDBC.Record
import           Database.Record (ToSql, FromSql)


instance ShowConstantTermsSQL Integer where
  showConstantTermsSQL' = let f :: Integer -> Int
                              f = fromInteger
                          in showConstantTermsSQL' . f


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



persist :: Lycopene a -> Persist a
persist (LProject p) = persistProject p

persistProject :: ProjectF a -> Persist a
persistProject (NewProjectF n d) = undefined
persistProject (AddProjectF p) = undefined
persistProject (RemoveProjectF p) = undefined
persistProject (UpdateProjectF f p) = undefined
persistProject (FetchByIdProjectF i) = undefined
persistProject (FetchByNameProjectF n) = undefined
persistProject FetchAllProjectF =
