module Lycopene.Option.Init (initProject) where


import           Options.Applicative
import           Lycopene.Option.Command
import           Lycopene.Configuration


initProject :: Configuration -> ParserInfo Command
initProject conf = 
  let initP = Init
        <$> optional (strOption (long "name" <> short 'n' <> metavar "NAME"))
        <*> optional (strOption (long "description" <> short 'd' <> metavar "DESC"))
        <*> strOption (long "file" <> short 'f' <> metavar "CONFIGFILE" <> value (projectConf conf))
  in info initP (progDesc "Initialize a directory as project base")

