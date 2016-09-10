module Lycopene.Command 
  ( runCommand
  ) where

import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Lycopene.Option.Command as Cmd
import qualified Lycopene.Configuration as Cfg
import qualified Lycopene.Core as Core
import           Lycopene.Application (defaultEngine)
import           Lycopene.Environment (dataPath)
import           Lycopene.Database (connect, runDB, rawDB, DBException(..))
import           Lycopene.Database.Relational (schema)
import           Lycopene.Web (startServer)

runCommand :: Cfg.Configuration -> Cmd.LycoCommand -> IO ()
runCommand cfg (Cmd.LycoCommand comm subcmd) = runSubcommand subcmd
  where
    runSubcommand :: Cmd.Command -> IO ()
    runSubcommand Cmd.Version   =
      putStrLn "dummy version"
    runSubcommand Cmd.Configure =
      dataPath >>= connect >>= runDB (rawDB schema) >>= printResult
    runSubcommand (Cmd.Start p) =
      defaultEngine >>= startServer p

printResult :: (Show a) => Either DBException a -> IO ()
printResult = putStrLn . show
