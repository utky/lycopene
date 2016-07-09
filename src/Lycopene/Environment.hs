module Lycopene.Environment where

import           Lycopene.Database
import           Lycopene.Core.Monad

createDatabase :: Lycopene ()
createDatabase = runPersist $ direct createTables
