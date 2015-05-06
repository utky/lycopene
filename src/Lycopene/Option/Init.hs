module Lycopene.Option.Init (initProject) where


import           Options.Applicative
import           Lycopene.Option.Command


initProject :: ParserInfo Command
initProject = 
  let initP = (\a b c -> Operation $ Init a b c)
        <$> optional (strOption (long "name" <> short 'n' <> metavar "NAME"))
        <*> optional (strOption (long "description" <> short 'd' <> metavar "DESC"))
        <*> strOption (long "path" <> short 'p' <> metavar "DIR" <> value ".")
  in info initP (progDesc "Initialize a directory as project base")

