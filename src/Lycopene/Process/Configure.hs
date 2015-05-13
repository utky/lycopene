module Lycopene.Process.Configure where

import qualified Data.Text as T
import           System.FilePath (dropFileName)
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           Lycopene.Process.Core (ProcessR, out, MonadIO, liftIO, debug, failure, runDomain)
import           Lycopene.Environment (createDatabase)
import           Lycopene.Core
import           Lycopene.Core.Project as Project
import           Lycopene.Core.Sprint as Sprint


configure :: (MonadIO m) => FilePath -> ProcessR m
configure target = do
  let createParentIfMissing = (createDirectoryIfMissing True) . dropFileName
  -- FIXME: debug print
  -- debug $ T.pack target
  exists <- liftIO $ createParentIfMissing target >> doesFileExist target
  case exists of
    True  -> (debug $ "already exists: " ++ target) >> failure
    False -> runDomain setupSchema >> (out $ T.pack target)

setupSchema :: LycopeneT Persist ()
setupSchema = do
  createDatabase
  _ <- Project.inbox
  _ <- Sprint.inboxDefault
  _ <- Sprint.inboxBacklog
  return ()
