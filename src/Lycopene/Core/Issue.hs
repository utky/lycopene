{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
module Lycopene.Core.Issue where

import           Data.Char (toLower)
import qualified Data.Text as T
import           GHC.Generics
import           Web.HttpApiData
import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.Aeson.Types
                   (genericToEncoding, genericParseJSON, Options(..)
                   , defaultOptions, withText)
import           Lycopene.Core.Scalar
import           Lycopene.Core.Identifier (generate, nameIdGen)
import           Lycopene.Core.Project (ProjectId)
import           Lycopene.Core.Sprint (SprintId)
import           Lycopene.Freer (Freer, liftR)

type IssueId = Identifier

data IssueStatus
  = IssueOpen
  | IssueClosed
  deriving (Show, Eq, Ord)

instance ToJSON IssueStatus where
  toJSON IssueOpen = toJSON "open"
  toJSON IssueClosed = toJSON "closed"
  
instance FromJSON IssueStatus where
  parseJSON = withText "open|closed" $ \t ->
    case T.unpack t of
      "open"   -> return IssueOpen
      "closed" -> return IssueClosed
      _          -> fail "open|closed"

instance FromHttpApiData IssueStatus where
  parseQueryParam = parse where
    parse t =
      case T.unpack t of
        "open"   -> Right IssueOpen
        "closed" -> Right IssueClosed
        _        -> Left $ T.pack "IssueStatus"


data Issue
    = Issue
    { issueId :: !IssueId
    , issueTitle :: !String
    , issueDescription :: !Description
    , issueStatus :: !IssueStatus
    } deriving (Show, Generic)

instance Eq Issue where
    x == y = (issueId x) == (issueId y)

issueOptions :: Options
issueOptions =
  defaultOptions
    { fieldLabelModifier = map toLower . drop (length "issue") }

instance ToJSON Issue where
  toEncoding = genericToEncoding issueOptions
instance FromJSON Issue where
  parseJSON = genericParseJSON issueOptions



-- | Aggregation of Issue use-case
data IssueF a where
  -- | Create new issue for the sprint.
  AddIssueF :: ProjectId -> SprintId -> Issue -> IssueF Issue
  -- |
  UpdateIssueStatusF :: IssueStatus -> IssueId -> IssueF Issue
  -- | Remove the issue from sprint.
  RemoveIssueF :: IssueId -> IssueF ()
  -- | Fetch current open issue.
  FetchByStatusIssueF :: SprintId -> IssueStatus -> IssueF [Issue]
  -- |
  FetchByIdIssueF :: IssueId -> IssueF Issue
  -- | Move the issue to given sprint.
  -- MoveIssue :: SprintId -> IssueId -> IssueEvent ()


type IssueM = Freer IssueF

newIssue :: String -> Description -> Issue
newIssue n d =
  let next = generate nameIdGen ("issue", n)
  in  Issue next n d IssueOpen

addIssue :: ProjectId -> SprintId -> Issue -> IssueM Issue
addIssue p s i = liftR $ AddIssueF p s i

fetchByStatusIssue :: ProjectId -> SprintId -> IssueStatus -> IssueM [Issue]
fetchByStatusIssue pj sp st = liftR $ FetchByStatusIssueF sp st

removeIssue :: IssueId -> IssueM ()
removeIssue = liftR . RemoveIssueF

fetchIssue :: IssueId -> IssueM Issue
fetchIssue = liftR . FetchByIdIssueF
