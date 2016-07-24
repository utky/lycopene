{-# LANGUAGE GADTs #-}
module Lycopene.Core.Sprint where

import           Lycopene.Core.Scalar
import           Lycopene.Freer (Freer, liftR, foldFreer)
import           Lycopene.Core.Store (Change)
import           Lycopene.Core.Project (ProjectId)
import           Lycopene.Core.Identifier (generate, nameIdGen)
import           Lycopene.Lens (Lens, set, field)

type SprintId = Identifier

data SprintStatus
  = Finished
  | Running
  deriving (Show, Eq, Ord)

-- | A time box which includes zero or more issues.
data Sprint
    = Sprint
    { sprintId :: !SprintId
    , sprintName :: !Name
    , sprintDescription :: !Description
    , sprintStartOn :: !(Maybe Date)
    , sprintEndOn :: !(Maybe Date)
    , sprintStatus :: !SprintStatus
    } deriving (Show)

instance Eq Sprint where
    x == y = (sprintId x) == (sprintId y)

-- Minimal lenses for Sprint
-- ==================================================================

_sprintStatus :: Lens Sprint SprintStatus
_sprintStatus = field sprintStatus (\a s -> s { sprintStatus = a })

_sprintStartOn :: Lens Sprint (Maybe Date)
_sprintStartOn = field sprintStartOn (\a s -> s { sprintStartOn = a })

_sprintEndOn :: Lens Sprint (Maybe Date)
_sprintEndOn = field sprintEndOn (\a s -> s { sprintEndOn = a })

-- | Aggregation of Sprint use-case
--
-- 1. Create the sprint automatically when new project created.
-- 2. Create the sprint in existing project by user with starting date and deadline.
-- 3. Modify starting date and deadline.
-- 4. List active sprints.
-- 4. Finish the sprint.
data SprintEvent a where
  -- | Create a new sprint for the project.
  NewSprint :: Name -> Description -> Maybe Date -> Maybe Date -> ProjectId -> SprintEvent SprintId
  -- | Create a new deafult sprint for the project.
  NewDefaultSprint :: ProjectId -> SprintEvent SprintId
  -- | Modify starting date of the sprint.
  UpdateStartOnSprint :: Date -> SprintId -> SprintEvent ()
  -- | Modify deadline date of the sprint.
  UpdateEndOnSprint :: Date -> SprintId -> SprintEvent ()
  -- | Modify the sprint status with given status (running or finished).
  UpdateSprintStatus :: SprintStatus -> SprintId -> SprintEvent ()
  -- | Remove the sprint.
  RemoveSprint :: SprintId -> SprintEvent ()
  -- | Fetch all sprint deriving from the project.
  FetchProjectAllSprint :: ProjectId -> SprintEvent [Sprint]
  -- | Fetch running springs for the project.
  FetchRunningSprint :: ProjectId -> SprintEvent [Sprint]

processSprintEvent :: SprintEvent a -> SprintM a
processSprintEvent (NewSprint n d s e i) =
  (fmap sprintId . addSprint) $ newSprint n d s e i
processSprintEvent (NewDefaultSprint i) =
  newDefaultSprint i
processSprintEvent (UpdateStartOnSprint d s) =
  updateSprint (set _sprintStartOn (Just d)) $ fetchByIdSprint s
processSprintEvent (UpdateEndOnSprint d s) =
  updateSprint (set _sprintEndOn (Just d)) $ fetchByIdSprint s
processSprintEvent (UpdateSprintStatus st s) =
  updateSprint (set _sprintStatus st) $ fetchByIdSprint s
processSprintEvent (RemoveSprint i) =
  removeSprint i
processSprintEvent (FetchProjectAllSprint p) =
  fetchAllSprint p
processSprintEvent (FetchRunningSprint p) =
  fetchByStatusSprint Running p


-- |
data SprintF a where
  -- |
  NewSprintF :: Name -> Description -> Maybe DateTime -> Maybe DateTime -> ProjectId -> SprintF Sprint
  -- |
  AddSprintF :: Sprint -> SprintF Sprint
  -- |
  RemoveSprintF :: Sprint -> SprintF Sprint
  -- |
  UpdateSprintF :: Change Sprint -> Sprint -> SprintF Sprint
  -- |
  FetchByIdSprintF :: SprintId -> SprintF Sprint
  -- |
  FetchAllSprintF :: ProjectId -> SprintF [Sprint]
  -- |
  FetchByStatusSprintF :: SprintStatus -> SprintF [Sprint]

type SprintM = Freer SprintF

-- FIXME: IMPLEMENT sprint primitives.
newSprint = undefined
addSprint = undefined
newDefaultSprint = undefined
updateSprint f = undefined
removeSprint = undefined
fetchByIdSprint = undefined
fetchAllSprint = undefined
fetchByStatusSprint = undefined
