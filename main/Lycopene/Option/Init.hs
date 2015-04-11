module Lycopene.Option.Init (initProject) where


import           Options.Applicative
import           System.FilePath
import           System.Directory

import           Lycopene.Core
import           Lycopene.Core.Project
import           Lycopene.Option.Command



initProject :: ParserInfo Command
initProject = info initP (progDesc "Initialize a directory as project base")
  where
    initP = undefined

