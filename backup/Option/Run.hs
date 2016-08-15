module Lycopene.Option.Run (runTimer) where


import           Options.Applicative
import           Lycopene.Option.Command
import           Lycopene.Option.Internal


runTimer :: ParserInfo Command
runTimer = 
  let runP = Run
        <$> argid
        <*> optional (option auto (long "duration" <> short 'd' <> metavar "MINUTES"))
        <*> optional (strOption (long "comment" <> short 'm' <> metavar "COMMENT"))
  in info runP (progDesc "Run the pomodoro timer")

