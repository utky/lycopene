module Lycopene.Core.Project
    ( 
    ) where

-- Temporary Importing
import Data.Conduit
import           Database.Persist.Sqlite
import Data.Text

import Lycopene.Core.Monad
import Lycopene.Core.Type
import Lycopene.Core.Persist
import Lycopene.Core.Entity (Project(..))

type ProjectNamePattern = String

matchProjectName :: String -> Project -> Bool
matchProjectName s p = s == projectName p

fastProject :: String -> Project
fastProject = flip Project Nothing

insertProject p = do
  runSqlite (pack ":memory:") $ insert p
