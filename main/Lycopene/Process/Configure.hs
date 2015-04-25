module Lycopene.Process.Configure where

import qualified Data.Text as T
import           Control.Monad.Trans (lift)
import           System.FilePath ((</>), dropFileName)
import           System.Directory (createDirectoryIfMissing, getHomeDirectory, doesFileExist)
import           Lycopene.Process.Core (Process, ProcessM, out, MonadIO, liftIO, debug, failure)
import           Lycopene.Environment (createDatabase)
import           Lycopene.Core
import           Lycopene.Core.Project as Project


configure :: FilePath -> Process
configure target = do
  let createParentIfMissing = (createDirectoryIfMissing True) . dropFileName
      tgtIO = (flip expandHome target) `fmap` getHomeDirectory
  debug $ T.pack target
  tgt <- liftIO tgtIO
  (liftIO . createParentIfMissing) tgt
  exists <- (liftIO . doesFileExist) tgt
  case exists of
    True  -> (debug $ T.pack ("already exists: " ++ tgt)) >> failure
    False -> lift (createDatabase >> Project.inbox) >> (out $ T.pack ("database created: " ++ tgt))


expandHome :: FilePath -> FilePath -> FilePath
expandHome home ('~':xs) = home ++ xs
expandHome home path     = path

