module Lycopene.Option.Project (project) where


import           Options.Applicative

import           Lycopene.Core
import           Lycopene.Option.Command



project :: ParserInfo LycoAction
project = info projectP (progDesc "operates project data") where
  projectP = mkAction listProject

