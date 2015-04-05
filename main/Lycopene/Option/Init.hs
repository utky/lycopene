module Lycopene.Option.Init (initProject) where


import           Options.Applicative
import           System.FilePath
import           System.Directory

import           Lycopene.Core
import           Lycopene.Core.Project
import           Lycopene.Option.Command



initProject :: ParserInfo LycoAction
initProject = info initP (progDesc "Initialize a directory as project base")
  where
    initP = mkAction . liftL initLycoDir

initLycoDir = do
  cd <- getCurrentDirectory
  td <- fmap (\d -> d </> ".lyco") cd
  exists <- doesDirectoryExist td
  case exists of
    True  -> return td
    False -> createDirectory td >> return td


