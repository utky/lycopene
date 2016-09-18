{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
module Lycopene.Core.Sprint where

import           Data.Char (toLower)
import qualified Data.Text as T
import           GHC.Generics
import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.Aeson.Types
                   (typeMismatch, Value(..), Parser
                   , genericToEncoding, genericParseJSON, Options(..)
                   , defaultOptions, withText)
import           Lycopene.Core.Scalar
import           Lycopene.Freer (Freer, liftR, foldFreer)
import           Lycopene.Core.Store (Change)
import           Lycopene.Core.Project (ProjectId)
import           Lycopene.Core.Identifier (generate, nameIdGen)
import           Lycopene.Lens (Lens, set, field)

type SprintId = Identifier

data SprintStatus
  = SprintFinished
  | SprintRunning
  deriving (Show, Eq, Ord)


instance ToJSON SprintStatus where
  toJSON SprintFinished = toJSON "finished"
  toJSON SprintRunning = toJSON "running"
  
instance FromJSON SprintStatus where
  parseJSON = withText "running|finished" $ \t ->
    case T.unpack t of
      "running"   -> return SprintRunning
      "finished" -> return SprintFinished
      _          -> fail "running|finished"

-- | A time box which includes zero or more issues.
data Sprint
    = Sprint
    { sprintId :: !SprintId
    , sprintName :: !Name
    , sprintDescription :: !Description
    , sprintStartOn :: !(Maybe Date)
    , sprintEndOn :: !(Maybe Date)
    , sprintStatus :: !SprintStatus
    } deriving (Show, Generic)

sprintOptions :: Options
sprintOptions =
  defaultOptions
    { fieldLabelModifier = map toLower . drop (length "sprint") }

instance ToJSON Sprint where
  toEncoding = genericToEncoding sprintOptions
instance FromJSON Sprint where
  parseJSON = genericParseJSON sprintOptions

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


-- |
data SprintF a where
  -- |
  AddSprintF :: ProjectId -> Sprint -> SprintF Sprint
  -- |
  AddDefaultSprintF :: ProjectId -> Sprint -> SprintF Sprint
  -- |
  RemoveSprintF :: Sprint -> SprintF Sprint
  -- |
  UpdateSprintF :: Change Sprint -> Sprint -> SprintF Sprint
  -- |
  FetchByNameSprintF :: Name -> Name -> SprintF Sprint
  -- |
  FetchByStatusSprintF :: Name -> SprintStatus -> SprintF [Sprint]

type SprintM = Freer SprintF

-- FIXME: IMPLEMENT sprint primitives.
newBacklog :: ProjectId -> SprintM Sprint
newBacklog p =
  let new = newSprint "backlog" Nothing Nothing Nothing
  in  liftR $ AddDefaultSprintF p new

newSprint :: Name -> Description -> Maybe Date -> Maybe Date -> Sprint
newSprint n d s e =
  let next = generate nameIdGen ("sprint", n)
  in  Sprint next n d s e SprintRunning

fetchByNameSprint :: Name -> Name -> SprintM Sprint
fetchByNameSprint pj sp = liftR $ FetchByNameSprintF pj sp

fetchByStatusSprint :: Name -> SprintStatus -> SprintM [Sprint]
fetchByStatusSprint pj st = liftR $ FetchByStatusSprintF pj st

addSprint = undefined
updateSprint f = undefined
removeSprint = undefined
fetchByIdSprint = undefined
fetchAllSprint = undefined
