{-# LANGUAGE GADTs #-}
module Lycopene.Core.Sprint where

import           Lycopene.Core.Scalar
import           Lycopene.Freer (Freer, liftR, foldFreer)
import           Lycopene.Core.Store (Change)
import           Lycopene.Core.Project (ProjectId)
import           Lycopene.Core.Identifier (generate, nameIdGen)

type SprintId = Identifier

data Sprint
    = Sprint
    { sprintId :: !SprintId
    , sprintName :: !Name
    , sprintDescription :: !Description
    , sprintStartOn :: !(Maybe Date)
    , sprintEndOn :: !(Maybe Date)
    } deriving (Show)

instance Eq Sprint where
    x == y = (sprintId x) == (sprintId y)

-- | Aggregation of Sprint use-case
data SprintEvent a where
  NewSprint :: Name -> Description -> Maybe Date -> Maybe Date -> SprintEvent Sprint

processSprintEvent :: Monad m => SprintEvent a -> m a
processSprintEvent = undefined


data SprintF a where
  NewSprintF :: Name -> Description -> Maybe DateTime -> Maybe DateTime -> SprintF Sprint
  AddSprintF :: SprintF Sprint -> SprintF Sprint
  RemoveSprintF :: SprintF Sprint -> SprintF Sprint
  FetchByIdSprintF :: SprintId -> SprintF Sprint
  FetchAllSprintF :: ProjectId -> SprintF [Sprint]

type SprintM = Freer SprintF
