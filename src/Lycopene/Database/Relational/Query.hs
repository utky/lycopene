{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE GADTs            #-}
module Lycopene.Database.Relational.Query
                ( persist
                ) where

import           Control.Monad.Except (throwError)
import           Database.Relational.Query
import           Database.Record.FromSql
import           Database.Record.ToSql
import           Database.HDBC
import           Database.HDBC.Record
import           Database.Relational.Query
import           Database.HDBC.Record
import           Database.Record (ToSql, FromSql)
import qualified Lycopene.Core as Core
import           Lycopene.Database.Persist
import qualified Lycopene.Database.Relational.Project as Pj
import qualified Lycopene.Database.Relational.Sprint as Sp
import qualified Lycopene.Database.Relational.Issue as Is
import           Lycopene.Database.Relational.Decode
import           Lycopene.Freer (foldFreer)


-- instance ShowConstantTermsSQL Integer where
--   showConstantTermsSQL' = let f :: Integer -> Int
--                               f = fromInteger
--                           in showConstantTermsSQL' . f


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

persist :: Core.Lycopene a -> DB a
persist = foldFreer persistLyco where
  persistLyco (Core.ProjectL m) = persistProject m
  persistLyco (Core.SprintL m) = persistSprint m
  persistLyco (Core.IssueL m) = persistIssue m

persistProject :: Core.ProjectF a -> DB a
persistProject (Core.AddProjectF p) = 
  p <$ db (insertQueryPersist (Pj.insertProject' p) ())

persistProject (Core.RemoveProjectF n) =
  () <$ db (deletePersist (Pj.deleteByName n) ())

persistProject (Core.UpdateProjectF f p) = undefined

persistProject (Core.FetchByNameProjectF n) =
  fromEntity =<< fetchOne =<< db (selectPersist Pj.selectByName n)

persistProject Core.FetchAllProjectF =
  mapM fromEntity =<< db (selectPersist Pj.project ())

persistSprint :: Core.SprintF a -> DB a
persistSprint (Core.AddDefaultSprintF p s) = do
  let pjid = Core.idStr p
      spid = Core.idStr $ Core.sprintId s
  _ <- db (insertQueryPersist (Sp.insertSprint' p s) ())
  _ <- db (insertPersist Sp.insertBacklogSprint (Sp.BacklogSprint pjid spid))
  return s

persistSprint (Core.AddSprintF p sp) =
  sp <$ db (insertQueryPersist (Sp.insertSprint' p sp) ())

persistSprint (Core.FetchByNameSprintF pj sp) = do
  p <- fetchOne =<< db (selectPersist Pj.selectByName pj)
  let pjid = Pj.projectId p
  fromEntity =<< fetchOne =<< db (selectPersist Sp.selectByProjectAndName (pjid, sp))

persistSprint (Core.FetchByStatusSprintF p st) =
  let ist = Sp.encodeStatus st
      param = (p, ist)
  in  mapM fromEntity =<< db (selectPersist Sp.selectByProjectAndStatus param)

persistIssue :: Core.IssueF a -> DB a
persistIssue (Core.AddIssueF s is) =
  is <$ db (insertQueryPersist (Is.insertIssue' s is) ())

persistIssue (Core.FetchByStatusIssueF s st) =
  let spid = Core.idStr s
      stat = Is.encodeStatus st
  in  mapM fromEntity =<< db (selectPersist Is.selectBySprint (spid, stat))

fromEntity :: (Decoder a b) => a -> DB b
fromEntity = handleE . decode
  where
    handleE (Right x) = return x
    handleE (Left e) = throwError $ DecodeE e

fetchOne :: [a] -> DB a
fetchOne (x:xs) = return x
fetchOne _ = throwError ResultSetEmpty

