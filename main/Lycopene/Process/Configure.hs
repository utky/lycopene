module Lycopene.Process.Configure where

import qualified Data.Text as T
import           System.FilePath ((</>), dropFileName)
import           System.Directory (createDirectoryIfMissing, getHomeDirectory, doesFileExist)
import           Lycopene.Process.Core (ProcessM, out, MonadIO, liftIO, debug)
import           Lycopene.Environment (createDatabase)
import           Lycopene.Core


configure :: (MonadIO m) => FilePath -> ProcessM m
configure target = do
  let createParentIfMissing = (createDirectoryIfMissing True) . dropFileName
      expandHome :: FilePath -> FilePath -> FilePath
      expandHome home ('~':xs) = home ++ xs
      expandHome home path     = path
      tgtIO = (flip expandHome target) `fmap` getHomeDirectory
  tgt <- liftIO tgtIO
  (liftIO . createParentIfMissing) tgt
  exists <- (liftIO . doesFileExist) tgt
  case exists of
    True  -> debug $ T.pack ("already exists: " ++ tgt)
    False -> out $ T.pack ("database created: " ++ tgt)
