module Lycopene.Environment where

import           Lycopene.Core.Database
import           Lycopene.Core.Monad

createDatabase :: Lycopene ()
createDatabase = runPersist $ direct createTables
