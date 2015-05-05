module Lycopene.Option.Init (initProject) where


import           Options.Applicative
import           System.FilePath
import           System.Directory

import           Lycopene.Core
import           Lycopene.Core.Project
import           Lycopene.Option.Command



initProject :: ParserInfo Command
initProject = 
  let initP = (Operation . Init) <$> strOption (long "dest" <> short 'd' <> metavar "DIR" <> value ".")
  in info initP (progDesc "Initialize a directory as project base")

