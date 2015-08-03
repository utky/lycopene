module Lycopene.Option.Sp (listSprints) where


import           Options.Applicative

import           Lycopene.Option.Command


listSprints :: ParserInfo Command
listSprints = info (pure Sp) (progDesc "List active sprints in current project")

