module Lycopene.Core
      ( module Lycopene.Core.Project
      , module Lycopene.Core.Sprint
      , module Lycopene.Core.Issue
      , module Lycopene.Core.Record
      , module Lycopene.Core.Scalar
      , module Lycopene.Core.Pure
      ) where

import           Lycopene.Core.Scalar
import           Lycopene.Core.Project
import           Lycopene.Core.Sprint
import           Lycopene.Core.Issue
import           Lycopene.Core.Record
import           Lycopene.Core.Pure

-- | A set of domain specific operation which emit value @a@.
data Lycopene a
  = LProject (ProjectM a)
--  | LSprint  (SprintM a)

-- | 
data Event a
  = EProject (ProjectEvent a)
--  | ESprint (SprintEvent a)

-- | Entry point of domain.
-- `process` returns commands of domain
process :: Event a -> Lycopene a
process (EProject x) = LProject $ processProjectEvent x
-- process (ESprint x) = LSprint $ processSprintEvent x

feedback :: (a -> Event b) -> Lycopene a -> Event b
feedback = undefined
