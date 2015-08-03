module Lycopene.Option.New (newIssue) where


import           Options.Applicative
import           Lycopene.Option.Command
import           Lycopene.Option.Internal


newIssue :: ParserInfo Command
newIssue = 
  let newP = New
        <$> argtext
        <*> optional (strOption (long "description" <> short 'd' <> metavar "DESC"))
  in info newP (progDesc "create issues into current project backlog")

