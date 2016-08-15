module Lycopene.Command where

import           Lycopene.Option.Command
import qualified Lycopene.Configuration as C
import           Lycopene.Environment (dataPath)
import           Lycopene.Database (Persist, DataSource, connect, runPersist, rawPersist)

runCommand :: C.Configuration -> LycoCommand -> IO ()
runCommand cfg (LycoCommand comm subcmd) = runSubcommand subcmd
  where
    runSubcommand :: Command -> IO ()
    runSubcommand Version   = putStrLn "dummy version"
    runSubcommand Configure = runPersist' $ rawPersist (C.schema cfg)

runPersist' :: Persist a -> IO a
runPersist' ps = do
  dpath <- dataPath
  ds <- connect dpath
  runPersist ps ds
