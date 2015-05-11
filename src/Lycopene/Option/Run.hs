module Lycopene.Option.Run (runTimer) where


import           Options.Applicative
import           Lycopene.Option.Command


runTimer :: ParserInfo Command
runTimer = 
  let runP = (\a b -> Operation $ Run a b)
        <$> optional (option auto (long "issue" <> short 'i' <> metavar "ID"))
        <*> optional (option auto (long "duration" <> short 'd' <> metavar "MINUTES"))
  in info runP (progDesc "Run the pomodoro timer")

