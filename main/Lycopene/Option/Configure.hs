module Lycopene.Option.Configure
    ( configureDB
    ) where 

import           Options.Applicative
import           System.FilePath

import           Lycopene.Core
import           Lycopene.Option.Command

configureDB :: ParserInfo Command
configureDB = info initP (progDesc "initialize database if it doesn't exist.") where
  initP = Configure <$> strOption (long "file" <> short 'f' <> metavar "FILE" <> value ("~" </> ".lyco" </> "issues.db"))

