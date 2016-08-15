module Lycopene.Action.Hs where

import           Lycopene.Action
import qualified Lycopene.Core.Record as Record


listHistories :: Integer -> Action [Record.Record]
listHistories = domain . Record.history
