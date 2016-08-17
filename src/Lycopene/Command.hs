module Lycopene.Command 
  ( runCommand
  ) where

import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Lycopene.Option.Command as Cmd
import qualified Lycopene.Configuration as Cfg
import qualified Lycopene.Core as Core
import           Lycopene.Environment (dataPath)
import           Lycopene.Database (DB, connect, runDB, rawDB, DBException(..))
import           Lycopene.Database.Relational (persist, schema)

runCommand :: Cfg.Configuration -> Cmd.LycoCommand -> IO ()
runCommand cfg (Cmd.LycoCommand comm subcmd) = runSubcommand subcmd
  where
    runSubcommand :: Cmd.Command -> IO ()
    runSubcommand Cmd.Version   =
      putStrLn "dummy version"
    runSubcommand Cmd.Configure =
      handleResult =<< runDatabse (rawDB schema)
    runSubcommand Cmd.Projects =
      handleResult =<< processEvent (Core.EProject Core.AllProject)

handleResult :: (Show a) => Either DBException a -> IO ()
handleResult = putStrLn . show

processEvent :: (MonadIO m) => Core.Event a -> m (Either DBException a)
processEvent = runDatabse . persist . Core.process

runDatabse :: (MonadIO m) => DB a -> m (Either DBException a)
runDatabse d = do
  dpath <- liftIO dataPath
  ds <- liftIO $ connect dpath
  runDB d ds

