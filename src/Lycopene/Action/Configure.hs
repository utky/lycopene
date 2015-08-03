module Lycopene.Action.Configure where

import           Lycopene.Action
import           Lycopene.Environment (createDatabase)
import           Lycopene.Core.Project as Project
import           Lycopene.Core.Sprint as Sprint

import           System.FilePath (dropFileName)

prepareConfigure :: FilePath -> Action ()
prepareConfigure target = do
  let parent = dropFileName target
  exist <- isDir parent
  if exist then return () else mkdir parent

-- | Attempt to create fresh database.
configure :: Action ()
configure = domain $ createDatabase
                     >> Project.inbox
                     >> Sprint.inboxDefault
                     >> Sprint.inboxBacklog
                     >> return ()
