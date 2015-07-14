{-# LANGUAGE RankNTypes #-}
module Lycopene.Process
    ( buildProcess
    , runProcess
    , Chunk
    , runCommand
    , module Lycopene.Process.Internal
    ) where


import           Lycopene.Option (LycoCommand(..), CommonOption(..), Command(..))
import           Lycopene.Process.Internal

import           Lycopene.Configuration (Configuration(..))
import           Lycopene.Print (Print, printA)

import           Lycopene.Action
import           Lycopene.Action.Version   (version)
import           Lycopene.Action.Configure (prepareConfigure, configure)
import           Lycopene.Action.Init      (initialize)
import           Lycopene.Action.Ls        (listIssues)
import           Lycopene.Action.New       (newIssue)
import           Lycopene.Action.Pj        (listProjects)

import           Control.Concurrent (threadDelay)
import           System.FilePath ((</>))

import Debug.Trace (trace)

processAction :: Action a -> Configuration -> Process' a
processAction action cfg = do
  e <- liftIO . handleResult $ runAction cfg action
  case e of
    (Left err) -> fatal $ show err
    (Right x)  -> yield x


process :: (Print a) => Action a -> Configuration -> Process' String
process action cfg = printer <-< processAction action cfg

processFor :: (Print a, Foldable f) => Action (f a) ->  Configuration -> Process' String
processFor action cfg = printer <-< for (processAction action cfg) each

type Chunk = Either String String

buildProcess :: LycoCommand -> Configuration -> Producer Chunk IO ()
buildProcess (LycoCommand commonOpt cmd) cfg@(Configuration home dp pj) =
  let mkPs Version   = process version
      mkPs Configure = process (prepareConfigure dp >> configure >> return dp)
      mkPs (Init mName mDesc path) = process $ initialize mName mDesc path
      mkPs (Ls showAll) = processFor $ listIssues showAll
      mkPs (New iTitle mDesc) = process $ newIssue iTitle mDesc
      mkPs Pj = processFor listProjects
      mkPs (Run mi md)  = \_ -> (liftIO $ threadDelay 3000000) >> info "Successfully end"
      mkPs _         = \_ -> fatal "No command specified"
  in runWriterPs $ mkPs cmd cfg
  
runProcess :: Producer Chunk IO () -> ProcessEnv -> Effect IO ()
runProcess ps (ProcessEnv hout herr hin) =
                    let cOut = handleOut hout
                        cErr = handleOut herr
                        out  = choice cErr cOut
                        dummy = "hoge"
                    in  ps >-> out

runCommand :: Configuration -> LycoCommand -> IO ()
runCommand cfg@(Configuration home dp pj) (LycoCommand commonOpt cmd) =
  let execAction :: Action () -> IO ()
      execAction = (>>= (either print return)) . handleResult . runAction cfg
      subcmd Version                 = execAction $ version >>= send
      subcmd Configure               = execAction $ (prepareConfigure dp >> configure >> return dp) >>= send
      subcmd (Init mName mDesc path) = execAction $ initialize mName mDesc path >>= send
      subcmd (Ls showAll)            = execAction ((listIssues showAll) >>= mapM_ send)
      subcmd (New iTitle mDesc)      = execAction $ newIssue iTitle mDesc >>= send
      subcmd Pj                      = execAction (listProjects >>= mapM_ send)
      subcmd (Run mi md)             = threadDelay 3000000 >> print "Successfully end"
  in  subcmd cmd

