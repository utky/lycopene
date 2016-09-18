module Lycopene.Core.Monad
  ( Lycopene
  , LycopeneF(..)
  , project
  , sprint
  , issue
  ) where

import           Control.Monad.Trans
import           Control.Monad.Reader
import           Control.Monad.Except
import           Lycopene.Freer (Freer, hoistFreer)
import           Lycopene.Core.Project (ProjectM, ProjectF)
import           Lycopene.Core.Sprint (SprintM, SprintF)
import           Lycopene.Core.Issue (IssueM, IssueF)

type Lycopene = Freer LycopeneF

-- | A set of domain specific operation which emit value @a@.
-- Maybe replacable to Extensible effect
data LycopeneF a
  = ProjectL (ProjectF a)
  | SprintL (SprintF a)
  | IssueL (IssueF a)

-- (Hoist m o, Hoist n o) => m a -> (a -> n b) -> o b

project :: ProjectM a -> Lycopene a
project = hoistFreer ProjectL

sprint :: SprintM a -> Lycopene a
sprint = hoistFreer SprintL

issue :: IssueM a -> Lycopene a
issue = hoistFreer IssueL
