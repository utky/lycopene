module Lycopene.Option.New (newIssue) where


import           Options.Applicative
import           Lycopene.Option.Command


newIssue :: ParserInfo Command
newIssue = 
  let newP = New
        <$> strOption (long "title" <> short 't' <> metavar "TITLE")
        <*> optional (strOption (long "description" <> short 'd' <> metavar "DESC"))
  in info newP (progDesc "create issues into current project backlog")

