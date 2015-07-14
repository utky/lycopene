module Lycopene.Option.Project (listProjects) where


import           Options.Applicative

import           Lycopene.Option.Command


listProjects :: ParserInfo Command
listProjects = info (pure Pj) (progDesc "List active projects")

