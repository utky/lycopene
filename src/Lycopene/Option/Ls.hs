module Lycopene.Option.Ls (listIssues) where


import           Options.Applicative
import           Lycopene.Option.Command


listIssues :: ParserInfo Command
listIssues = 
  let lsP = Ls <$> switch (long "all" <> short 'a')
  in info lsP (progDesc "list all open issues in current project")


