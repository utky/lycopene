module Lycopene.Core
      ( Lycopene(..)
      , process
      , module Lycopene.Core.Project
      , module Lycopene.Core.Sprint
      , module Lycopene.Core.Issue
      , module Lycopene.Core.Record
      , module Lycopene.Core.Scalar
      , module Lycopene.Core.Pure
      , module Lycopene.Core.Event
      , module Lycopene.Core.Monad
      ) where

import           Lycopene.Core.Event
import           Lycopene.Core.Scalar
import           Lycopene.Core.Project
import           Lycopene.Core.Sprint
import           Lycopene.Core.Issue
import           Lycopene.Core.Record
import           Lycopene.Core.Pure
import           Lycopene.Core.Monad


-- | Entry point of domain.
-- `process` returns commands of domain
process :: Event a -> Lycopene a
process (EProject x) = processProjectEvent x
-- process (ESprint x) = LSprint $ processSprintEvent x
