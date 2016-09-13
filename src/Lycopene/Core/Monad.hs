module Lycopene.Core.Monad
  ( Lycopene
  , LycopeneF(..)
  , module Lycopene.Freer
  ) where

import           Control.Monad.Trans
import           Control.Monad.Reader
import           Control.Monad.Except
import           Lycopene.Freer (Freer, hoistFreer)
import           Lycopene.Core.Project (ProjectF)

type Lycopene = Freer LycopeneF

-- | A set of domain specific operation which emit value @a@.
-- Maybe replacable to Extensible effect
data LycopeneF a
  = ProjectL (ProjectF a)
