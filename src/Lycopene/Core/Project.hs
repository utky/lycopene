module Lycopene.Core.Project
    ( listProject
    ) where

import           Database.Persist.Sqlite

import           Lycopene.Core.Monad
import           Lycopene.Core.Persist
import           Lycopene.Core.Entity (Project(..))

type ProjectNamePattern = String

matchProjectName :: String -> Project -> Bool
matchProjectName s p = s == projectName p

fastProject :: String -> Project
fastProject = flip Project Nothing


listProject :: LycoApp [Project]
listProject = do
    selected <- runDB $ selectList [] []
    return $ fmap entityVal selected
