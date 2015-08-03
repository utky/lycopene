{-# LANGUAGE RankNTypes #-}
module Lycopene.Process
    ( runCommand
    -- , buildProcess
    -- , runProcess
    -- , Chunk
    -- , module Lycopene.Process.Internal
    ) where


import           Lycopene.Option (LycoCommand(..), Command(..))

import           Lycopene.Configuration (Configuration(..))

import           Lycopene.Action
import           Lycopene.Action.Version   (version)
import           Lycopene.Action.Configure (prepareConfigure, configure)
import           Lycopene.Action.Init      (initialize)
import           Lycopene.Action.Ls        (listIssues)
import           Lycopene.Action.New       (newIssue)
import           Lycopene.Action.Pj        (listProjects)
import           Lycopene.Action.Sp        (listSprints)
import           Lycopene.Action.Run       (record)
import           Lycopene.Action.Hs        (listHistories)
import           Lycopene.Action.Done      (doneIssue)

import           Control.Concurrent (threadDelay)
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone, LocalTime)

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

runCommand :: Configuration -> LycoCommand -> IO ()
runCommand cfg (LycoCommand commonOpt cmd) =
  let execAction :: Action () -> IO ()
      execAction = (>>= either print return) . handleResult . runAction cfg
      subcmd Version                 = execAction $ version >>= send
      subcmd Configure               = execAction $ (prepareConfigure (datapath cfg) >> configure >> return (datapath cfg)) >>= send
      subcmd (Init mName mDesc path) = execAction $ initialize mName mDesc path >>= send
      subcmd (Ls showAll)            = execAction (listIssues showAll >>= mapM_ send)
      subcmd (New iTitle mDesc)      = execAction $ newIssue iTitle mDesc >>= send
      subcmd Pj                      = execAction (listProjects >>= mapM_ send)
      subcmd Sp                      = execAction (listSprints >>= mapM_ send)
      subcmd (Hs i)                  = execAction (listHistories i >>= mapM_ send)
      subcmd (Run i md mc)           = do
        clt <- getCurrentLocalTime
        threadDelay (pomodoroMinutes cfg)
        execAction (record i clt)
        putStrLn "Successfully end"
      subcmd (Done i)                = execAction (doneIssue i)
  in  subcmd cmd

