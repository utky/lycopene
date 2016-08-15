module Lycopene.Action.Sp where

import           Lycopene.Action
import qualified Lycopene.Core.Sprint as Sprint


listSprints :: Action [Sprint.Sprint]
listSprints = domain Sprint.sprintByProject
