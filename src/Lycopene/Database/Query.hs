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


instance ShowConstantTermsSQL Integer where
  showConstantTermsSQL' = let f :: Integer -> Int
                              f = fromInteger
                          in showConstantTermsSQL' . f

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
