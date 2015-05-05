module Lycopene.Pretty (Pretty(..)) where

import qualified Lycopene.Core.Project as P
import qualified Lycopene.Core.Sprint as S
import qualified Lycopene.Core.Issue as I
import qualified Lycopene.Core.Record as R

import qualified Data.Text as T

class Pretty a where
  print :: a -> T.Text

