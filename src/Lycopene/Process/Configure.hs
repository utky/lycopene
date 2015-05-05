module Lycopene.Process.Configure where

import qualified Data.Text as T
import           Control.Monad.Trans (lift)
import           System.FilePath ((</>), dropFileName)
import           System.Directory (createDirectoryIfMissing, getHomeDirectory, doesFileExist)
import           Lycopene.Process.Core (ProcessR, Result, out, MonadIO, liftIO, debug, failure, runDomain)
import           Lycopene.Environment (createDatabase)
import           Lycopene.Core
import           Lycopene.Core.Project as Project


configure :: (MonadIO m) => FilePath -> ProcessR m
configure target = do
  let createParentIfMissing = (createDirectoryIfMissing True) . dropFileName
  -- FIXME: debug print
  -- debug $ T.pack target
  exists <- liftIO $ createParentIfMissing target >> doesFileExist target
  case exists of
    True  -> (debug $ T.pack ("already exists: " ++ target)) >> failure
    False -> runDomain (createDatabase) >> (out $ T.pack ("database created: " ++ target))

