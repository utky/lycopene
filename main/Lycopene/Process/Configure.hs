module Lycopene.Process.Configure where

import           System.FilePath ((</>), dropFileName)
import           System.Directory (createDirectoryIfMissing)
import           Lycopene.Process.Core (ProcessM, out, MonadIO)
import           Lycopene.Core


configure :: (MonadIO m) => FilePath -> ProcessM m
configure target = do
  let createParentIfMissing = createDirectoryIfMissing . dropFileName
  createParentIfMissing target
  hd <- getHomeDirectory
  hd </> ".lyco"
 
