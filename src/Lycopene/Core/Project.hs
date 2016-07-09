{-# LANGUAGE GADTs #-}
module Lycopene.Core.Project
  ( ProjectEvent(..)
  , processProjectEvent
  , Project(..)
  , ProjectId
  , ProjectF(..)
  , ProjectM
  , newProject
  , addProject
  , removeProject
  , updateProject
  , fetchByNameProject
  , fetchByIdProject
  , fetchAllProject
  , activateProject
  , deactivateProject
  , runProjectPure
  ) where

import           Lycopene.Core.Scalar
import           Lycopene.Freer (Freer, liftR, foldFreer)
import           Lycopene.Core.Store (Change)
import           Lycopene.Core.Identifier (generate, nameIdGen)
import           Lycopene.Core.Pure (VStore, VResult, runVStore, initial, values, save, fetch, remove, catch)
import           Lycopene.Lens (Lens, set, field)

type ProjectId = Identifier

data ProjectStatus 
  = Inactive -- ^ Indicate a project not proceeding or completed.
  | Active -- ^ Indicate a project is working in progress.
  deriving (Eq, Ord, Show)

data Project
  = Project
  { projectId   :: !ProjectId
  , projectName :: !Name
  , projectDescription :: Description
  , projectStatus :: !ProjectStatus
  }
  deriving (Show)

-- Minimal lenses for Project
_projectStatus :: Lens Project ProjectStatus
_projectStatus = field projectStatus (\a s -> s { projectStatus = a })

-- Project is identified by `projectId`
instance Eq Project where
  x == y = (projectId x) == (projectId y)

-- Use cases of Project
-- =======================================

-- | Aggregation of Project use-case
-- Lift ordinary values to Project semantices
data ProjectEvent a where
  -- | Lift Name and Description into a volatile entity.
  NewProject :: Name -> Description -> ProjectEvent ProjectId
  -- | Fetch a project which identified by ProjectId
  FetchProject :: ProjectId -> ProjectEvent Project
  -- | Fetch list of active project
  ActiveProject :: ProjectEvent [Project]
  -- | Fetch list of all project regardless of its status.
  AllProjectEvent :: ProjectEvent [Project]
  -- |
  DeactivateProject :: ProjectId -> ProjectEvent ()
  -- |
  ActivateProject :: ProjectId -> ProjectEvent ()
  -- | 
  UpdateProjectName :: Name -> ProjectId ->  ProjectEvent Project
  -- |
  UpdateProjectDescription :: Description -> ProjectId -> ProjectEvent Project
  -- |
  RemoveProject :: ProjectId -> ProjectEvent ()

-- | 
processProjectEvent :: ProjectEvent a -> ProjectM a
processProjectEvent (NewProject n d) = fmap projectId $ newProject n d
processProjectEvent (FetchProject i) = fetchByIdProject i
-- FIXME: eliminate in-memory filtering
processProjectEvent ActiveProject= fmap (filter ((== Active) . projectStatus)) fetchAllProject
processProjectEvent AllProjectEvent = fetchAllProject
processProjectEvent (DeactivateProject i) = () <$ deactivateProject (fetchByIdProject i)

-- | Operational primitives of Project
--
-- ProjectF Projectという型は実質的にただの参照な気がするな
--
-- 7/9 ProjectFを要求する再帰的な構造を捨てる
-- つまり ProjectF a -> ProjectF a を
-- a -> ProjectF a
-- にした
-- これによりFlatなデータを持ち上げるだけのコンストラクタになる
-- AST的にはleafしかない。nodeはFreeが作る
--
-- :<$> :: (a -> b) -> f a -> f b
-- :>>= :: (a -> m b) -> m a -> m b
-- :>> :: m a -> m b -> m b
-- :<< :: m a -> m b -> m a
--
-- ((Remove (Update f (Add (New n d)))) :>> (Fetch n))
-- (f :<$> (Fetch n d))
-- (Update f (Fetch (Name "hoge")))
data ProjectF a where
  NewProjectF :: Name -> Description -> ProjectF Project
  AddProjectF :: Project -> ProjectF Project
  RemoveProjectF :: Project -> ProjectF Project
  UpdateProjectF :: Change Project -> Project -> ProjectF Project
  -- | 取れない場合はエラーなのでここでは考慮しない
  FetchByIdProjectF :: ProjectId -> ProjectF Project
  FetchByNameProjectF :: Name -> ProjectF Project
  FetchAllProjectF :: ProjectF [Project]

type ProjectM = Freer ProjectF

-- Lifting boiler plate

newProject :: Name -> Description -> ProjectM Project
newProject n d = liftR $ NewProjectF n d

addProject :: ProjectM Project -> ProjectM Project
addProject = (>>= (liftR . AddProjectF))

removeProject :: ProjectM Project -> ProjectM Project
removeProject = (>>= (liftR . RemoveProjectF))

updateProject :: Change Project -> ProjectM Project -> ProjectM Project
updateProject f = (>>= (liftR . UpdateProjectF f))

fetchByIdProject :: ProjectId -> ProjectM Project
fetchByIdProject = liftR . FetchByIdProjectF

fetchByNameProject :: Name -> ProjectM Project
fetchByNameProject = liftR . FetchByNameProjectF

fetchAllProject :: ProjectM [Project]
fetchAllProject = liftR FetchAllProjectF

activateProject :: ProjectM Project -> ProjectM Project
activateProject = (>>= (liftR . UpdateProjectF (set _projectStatus Active)))

deactivateProject :: ProjectM Project -> ProjectM Project
deactivateProject = (>>= (liftR . UpdateProjectF (set _projectStatus Inactive)))

-- | Translate AST into State manipulations
runPure' :: ProjectF a -> VStore ProjectId Project a
runPure' (NewProjectF n d) = return $ newProject' n d 
runPure' (AddProjectF x) = fmap (const x) (save (projectId x) x)
runPure' FetchAllProjectF = values
runPure' (UpdateProjectF f x) = return $ f x
runPure' (FetchByIdProjectF i) = fetch i
runPure' (FetchByNameProjectF n) = 
    let nameEq = (== n) . projectName
        headMaybe [] = Nothing
        headMaybe (x:xs) = Just x
    in values >>= (catch . headMaybe . filter nameEq)
runPure' (RemoveProjectF x) = (remove (projectId x)) >> (return x)


runProjectPure :: ProjectM a -> VResult a
runProjectPure = runVStore initial . foldFreer runPure'

-- | Construct a Project
newProject' :: Name -> Description -> Project
newProject' n d =
  let next = generate nameIdGen ("project", n)
  in  Project
        { projectId   = next
        , projectName = n
        , projectDescription = d
        , projectStatus = Active
        }
