module Lycopene.Process.Configure where

import qualified Data.Text as T
import           Control.Monad.Trans (lift)
import           System.FilePath ((</>), dropFileName)
import           System.Directory (createDirectoryIfMissing, getHomeDirectory, doesFileExist)
import           Lycopene.Process.Core (ProcessR, Result, out, MonadIO, liftIO, debug, failure, runDomain)
import           Lycopene.Environment (createDatabase)
import           Lycopene.Core
import           Lycopene.Core.Project as Project
import           Lycopene.Core.Sprint as Sprint


configure :: (MonadIO m) => FilePath -> ProcessR m
configure target = do
  let createParentIfMissing = (createDirectoryIfMissing True) . dropFileName
      dbOperation = createDatabase >> Project.inbox >> Sprint.inboxDefault >> Sprint.inboxBacklog >> return ()

  -- FIXME: debug print
  -- debug $ T.pack target
  exists <- liftIO $ createParentIfMissing target >> doesFileExist target
  case exists of
    True  -> (debug $ T.pack ("already exists: " ++ target)) >> failure
    False -> runDomain dbOperation >> (out $ T.pack ("database created: " ++ target))

