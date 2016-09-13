{-# LANGUAGE GADTs #-}
-- | Define request messages to communicate with domain.
module Lycopene.Core.Event where

import           Lycopene.Core.Scalar
import           Lycopene.Core.Monad
import qualified Lycopene.Core.Project as Project

-- | 
data Event a
  = EProject (ProjectEvent a)
--  | ESprint (SprintEvent a)

-- Use cases of Project
-- =======================================

-- | Aggregation of Project use-case
-- Lift ordinary values to Project semantices
data ProjectEvent a where
  -- | Lift Name and Description into a volatile entity.
  NewProject :: Name -> Description -> ProjectEvent Project.Project
  -- | Fetch a project which identified by ProjectId
  FetchProject :: Name -> ProjectEvent Project.Project
  -- | Fetch list of all project regardless of its status.
  AllProject :: ProjectEvent [Project.Project]
  -- |
  RemoveProject :: Name -> ProjectEvent ()
  -- | Fetch list of active project
  -- ActiveProject :: ProjectEvent [Project]
  -- |
  -- DeactivateProject :: ProjectId -> ProjectEvent ()
  -- |
  -- ActivateProject :: ProjectId -> ProjectEvent ()
  -- | 
  -- UpdateProjectName :: Name -> ProjectId ->  ProjectEvent Project
  -- |
  -- UpdateProjectDescription :: Description -> ProjectId -> ProjectEvent Project

-- | 
processProjectEvent :: ProjectEvent a -> Lycopene a
processProjectEvent (NewProject n d) =
  hoistFreer ProjectL $ Project.addProject $ Project.newProject n d
processProjectEvent (RemoveProject n) =
  () <$ (hoistFreer ProjectL $ Project.removeProject n)
processProjectEvent (FetchProject n) =
  hoistFreer ProjectL $ Project.fetchByNameProject n
processProjectEvent AllProject =
  hoistFreer ProjectL $ Project.fetchAllProject
-- FIXME: eliminate in-memory filtering
-- processProjectEvent ActiveProject =
--   fmap (filter ((== ProjectActive) . (get _status))) fetchAllProject
-- processProjectEvent (DeactivateProject i) =
--   () <$ deactivateProject (fetchByIdProject i)
