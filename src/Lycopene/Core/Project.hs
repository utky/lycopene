{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
module Lycopene.Core.Project
  ( ProjectEvent(..)
  , processProjectEvent
  , Project(Project)
  , ProjectStatus(..)
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

import           Prelude hiding (id)
import           GHC.Generics
import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)
import           Data.Aeson.Types (typeMismatch, Value(..), Parser)
import           Lycopene.Core.Scalar
import           Lycopene.Freer (Freer, liftR, foldFreer)
import           Lycopene.Core.Store (Change)
import           Lycopene.Core.Identifier (generate, nameIdGen)
import           Lycopene.Core.Pure (VStore, VResult, runVStore, initial, values, save, fetch, remove, catch)
import           Lycopene.Lens (Lens, set, get, field)

type ProjectId = Identifier

data ProjectStatus 
  = ProjectInactive -- ^ Indicate that a project is not proceeding or completed.
  | ProjectActive -- ^ Indicate a project is working in progress.
  deriving (Eq, Ord, Show)

instance ToJSON ProjectStatus where
  toJSON ProjectInactive = toJSON (0 :: Int)
  toJSON ProjectActive = toJSON (1 :: Int)

instance FromJSON ProjectStatus where
  parseJSON j@(Number x) = 
    let withInteger :: Integer -> Parser ProjectStatus
        withInteger 0 = return ProjectInactive
        withInteger 1 = return ProjectActive
        withInteger y = typeMismatch "0 or 1" j
    in  parseJSON j >>= withInteger
  parseJSON invalid = typeMismatch "Integer" invalid

data Project
  = Project
  { id   :: !ProjectId
  , name :: !Name
  , description :: Description
  , status :: !ProjectStatus
  }
  deriving (Show, Generic)

instance ToJSON Project
instance FromJSON Project

-- Project is identified by `projectId`
instance Eq Project where
  x == y = (id x) == (id y)

-- Minimal lenses for Project
-- ==================================================================

_id :: Lens Project ProjectId
_id = field id (\a s -> s { id = a })

_name :: Lens Project Name
_name = field name (\a s -> s { name = a })

_status :: Lens Project ProjectStatus
_status = field status (\a s -> s { status = a })

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
  AllProject :: ProjectEvent [Project]
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
processProjectEvent (NewProject n d) =
  (fmap id . addProject) $ newProject n d
processProjectEvent (FetchProject i) =
  fetchByIdProject i
-- FIXME: eliminate in-memory filtering
processProjectEvent ActiveProject =
  fmap (filter ((== ProjectActive) . (get _status))) fetchAllProject
processProjectEvent AllProject =
  fetchAllProject
processProjectEvent (DeactivateProject i) =
  () <$ deactivateProject (fetchByIdProject i)

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
-- いやここはADTではなく関数でやればいいけどそのためには
-- ADTがMonadを要求するので結局Freerとかを使うことになる
--
-- が、仮に eval :: Monad m => f a -> m a
-- であればADTがMonad則を満たしていなくてもよいかも。
-- bind, joinなどは m a に委譲すればいいので。
--
-- ((Remove (Update f (Add (New n d)))) :>> (Fetch n))
-- (f :<$> (Fetch n d))
-- (Update f (Fetch (Name "hoge")))
data ProjectF a where
  NewProjectF :: ProjectId -> Name -> Description -> ProjectF Project
  AddProjectF :: Project -> ProjectF Project
  RemoveProjectF :: Project -> ProjectF Project
  UpdateProjectF :: Change Project -> Project -> ProjectF Project
  FetchByIdProjectF :: ProjectId -> ProjectF Project
  FetchByNameProjectF :: Name -> ProjectF Project
  FetchAllProjectF :: ProjectF [Project]

type ProjectM = Freer ProjectF

-- Lifting boiler plate

newProject :: Name -> Description -> ProjectM Project
newProject n d =
  let next = generate nameIdGen ("project", n)
  in  liftR $ NewProjectF next n d

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
activateProject = (>>= (liftR . UpdateProjectF (set _status ProjectActive)))

deactivateProject :: ProjectM Project -> ProjectM Project
deactivateProject = (>>= (liftR . UpdateProjectF (set _status ProjectInactive)))

-- | Translate AST into State manipulations
runPure' :: ProjectF a -> VStore ProjectId Project a
runPure' (NewProjectF i n d) = return $ Project i n d ProjectActive
runPure' (AddProjectF x) = fmap (const x) (save (id x) x)
runPure' FetchAllProjectF = values
runPure' (UpdateProjectF f x) = return $ f x
runPure' (FetchByIdProjectF i) = fetch i
runPure' (FetchByNameProjectF n) = 
    let nameEq = (== n) . name
        headMaybe [] = Nothing
        headMaybe (x:xs) = Just x
    in values >>= (catch . headMaybe . filter nameEq)
runPure' (RemoveProjectF x) = (remove (id x)) >> (return x)

runProjectPure :: ProjectM a -> VResult a
runProjectPure = runVStore initial . foldFreer runPure'

-- | Construct a Project
newProject' :: Name -> Description -> Project
newProject' n d =
  let next = generate nameIdGen ("project", n)
  in  Project next n d ProjectActive
