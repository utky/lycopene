module Lycopene.Option.Done (doneIssue) where


import           Options.Applicative
import           Lycopene.Option.Command
import           Lycopene.Option.Internal


doneIssue :: ParserInfo Command
doneIssue = 
  let doneP = Done <$> argid
  in info doneP (progDesc "Close an issue and set it has been done")

